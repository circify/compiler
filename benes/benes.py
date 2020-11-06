#!/usr/bin/python
#
# arbitrary-sized Waksman network experimental impl
# (C) 2020 Riad S. Wahby

import sys
if sys.version_info[0] != 3:
    raise RuntimeError("python3 required")

class BenesNetworkBase(object):
    num_inputs = 0
    ord_out = None
    sw = None

    def __init__(self, num_inputs):
        self.num_inputs = num_inputs

    def reset(self):
        self.ord_out = None
        self.sw = None

    def _set_order(self, ord_in, ord_out):
        assert len(ord_in) == self.num_inputs   # ord_in has right number of values
        assert len(ord_in) == len(set(ord_in))  # ord_in values are unique
        assert set(ord_in) == set(ord_out)      # ord_out is permutation of ord_in
        # rewrite ord_out in terms of input indices
        # this lets us pass lists of arbitrary values for ord_in and ord_out
        reord = dict(zip(ord_in, range(0, len(ord_in))))
        self.ord_out = [ reord[v] for v in ord_out ]

    def set_switches(self, ord_in, ord_out):
        assert self.sw is None, "set_switches called twice"
        self._set_order(ord_in, ord_out)

    def route(self, inputs):
        assert self.sw is not None
        assert len(inputs) == self.num_inputs
        return [ inputs[idx] for idx in self.ord_out ]

class BenesNetwork2x2(BenesNetworkBase):
    def __init__(self):
        super().__init__(2)

    def set_switches(self, ord_in, ord_out):
        super().set_switches(ord_in, ord_out)
        self.sw = self.ord_out[0] != 0

    def route(self, inputs):
        r_exp = super().route(inputs)
        if self.sw:
            r = [inputs[1], inputs[0]]
        else:
            r = list(inputs)
        assert r_exp == r
        return r

class BenesNetwork3x3(BenesNetworkBase):
    def __init__(self):
        super().__init__(3)

    #              t0
    # --|---|-------------|---|--
    #   | I |             | O |
    # --|---|--+       +--|---|--
    #          |       |
    #        m +-|---|-+ t1
    #            | M |
    # -----------|---|-----------
    def set_switches(self, ord_in, ord_out):
        super().set_switches(ord_in, ord_out)

        if self.ord_out[2] != 2:
            sw_m = True
            sw_i = self.ord_out[2] == 0
            sw_o = self.ord_out[1] != 2
        else:
            sw_m = False
            sw_i = False
            sw_o = self.ord_out[0] == 1

        self.sw = [sw_i, sw_m, sw_o]

    def route(self, inputs):
        r_exp = super().route(inputs)
        r = [None] * 3
        (sw_i, sw_m, sw_o) = self.sw

        (t0, m) = inputs[1::-1] if sw_i else inputs[:2]
        (t1, r[2]) = (inputs[2], m) if sw_m else (m, inputs[2])
        (r[0], r[1]) = (t1, t0) if sw_o else (t0, t1)
        assert None not in r
        assert r_exp == r
        return r

class BenesNetwork(BenesNetworkBase):
    top = None
    bot = None
    sw = None

    def __init__(self, num_inputs):
        super().__init__(num_inputs)
        assert num_inputs >= 4, "BenesNetwork requires at least 4 inputs"
        size_top = num_inputs // 2
        size_bot = num_inputs - size_top
        insts = [None, None]
        for (idx, size) in enumerate((num_inputs // 2, (num_inputs + 1) // 2)):
            if size == 2:
                insts[idx] = BenesNetwork2x2()
            elif size == 3:
                insts[idx] = BenesNetwork3x3()
            else:
                insts[idx] = BenesNetwork(size)
        (self.top, self.bot) = insts

    def reset(self):
        super().reset()
        self.top.reset()
        self.bot.reset()

    def set_switches(self, ord_in, ord_out):
        super().set_switches(ord_in, ord_out)
        ord_out = self.ord_out

        rev_map = [None] * self.num_inputs
        for (idx, val) in enumerate(ord_out):
            rev_map[val] = idx
        is_odd = self.num_inputs % 2 == 1
        iolen = self.num_inputs // 2
        sw_i = [None] * iolen
        sw_o = [None] * (iolen - 1 + is_odd)

        # queue: [ (input_idx, is_top) ]
        invals = set(range(0, self.num_inputs))
        if is_odd:
            invals.discard(self.num_inputs - 1)
            invals.discard(ord_out[-1])
            queue = [ (ord_out[-1], False), (self.num_inputs - 1, False) ]
        else:
            invals.discard(ord_out[-1])
            invals.discard(ord_out[-2])
            queue = [ (ord_out[-1], False), (ord_out[-2], True) ]
        while queue or invals:
            if not queue:
                # if queue is empty, arbitrarily choose an untouched input
                nextval = invals.pop()
                queue.append((nextval, nextval % 2 == 0))
            (idx_i, top_i) = queue.pop()
            swnum_i = idx_i // 2

            # figure out the value for the switch on the input side; set if necessary
            swval_i = (idx_i % 2 == 0) ^ top_i
            if swnum_i >= iolen:
                assert is_odd and swnum_i == iolen, "error: swnum_i overrun"
                assert swval_i == True, "error: tried to set final input switch"
                continue
            if sw_i[swnum_i] is not None:
                assert sw_i[swnum_i] == swval_i, "set_switches error: swval_i"
                continue
            sw_i[swnum_i] = swval_i

            # propagate the constraint for the other side of the switch, if necessary
            nidx_i = idx_i ^ 1
            assert nidx_i < self.num_inputs, "nidx_i out of range"
            invals.discard(nidx_i)
            idx_o = rev_map[nidx_i]
            swnum_o = idx_o // 2
            top_o = not top_i
            swval_o = (idx_o % 2 == 0) ^ top_o

            # figure out the value of the switch on the output side; set if necessary
            if swnum_o == iolen - 1 + is_odd:
                assert swval_o == False ^ is_odd, "error: tried to set final output switch"
                continue
            if sw_o[swnum_o] is not None:
                assert sw_o[swnum_o] == swval_o, "set_switches error: swval_o"
                continue
            sw_o[swnum_o] = swval_o

            # enqueue the new constraint
            nidx_o = idx_o ^ 1
            assert nidx_o < self.num_inputs, "nidx_o out of range"
            if ord_out[nidx_o] in invals:
                queue.append((ord_out[nidx_o], not top_o))
                invals.discard(ord_out[nidx_o])

        self.sw = sw_i + sw_o
        # make sure we're done
        assert None not in self.sw

        top_i = [None] * self.top.num_inputs
        top_o = list(top_i)
        bot_i = [None] * self.bot.num_inputs
        bot_o = list(bot_i)

        if is_odd:
            bot_i[-1] = self.num_inputs - 1
            bot_o[-1] = ord_out[-1]

        sw_o += [False]
        for idx in range(0, iolen):
            (i0, i1) = (2 * idx, 2 * idx + 1)
            (o0, o1) = (ord_out[i0], ord_out[i1])
            (top_i[idx], bot_i[idx]) = (i1, i0) if sw_i[idx] else (i0, i1)
            (top_o[idx], bot_o[idx]) = (o1, o0) if sw_o[idx] else (o0, o1)

        assert sorted(top_i) == sorted(top_o)
        assert sorted(bot_i) == sorted(bot_o)

        self.top.set_switches(top_i, top_o)
        self.bot.set_switches(bot_i, bot_o)

    def route(self, inputs):
        r_exp = super().route(inputs)

        top_i = [None] * self.top.num_inputs
        bot_i = [None] * self.bot.num_inputs

        iolen = self.num_inputs // 2
        is_odd = self.num_inputs % 2 == 1
        sw = self.sw + [False]
        sw_o = sw[iolen:]
        sw_i = sw[:iolen]
        assert sw == sw_i + sw_o

        if is_odd:
            bot_i[-1] = inputs[-1]
        for idx in range(0, iolen):
            (i0, i1) = (inputs[2 * idx], inputs[2 * idx + 1])
            (top_i[idx], bot_i[idx]) = (i1, i0) if sw_i[idx] else (i0, i1)
        assert None not in top_i and None not in bot_i

        top_o = self.top.route(top_i)
        bot_o = self.bot.route(bot_i)

        r = [None] * self.num_inputs
        if is_odd:
            r[-1] = bot_o[-1]
        for idx in range(0, iolen):
            (o0, o1) = (top_o[idx], bot_o[idx])
            (r[2 * idx], r[2 * idx + 1]) = (o1, o0) if sw_o[idx] else (o0, o1)
        assert None not in r
        assert r == r_exp
        return r

if __name__ == "__main__":
    import random
    for _ in range(0, 32):
        length = random.randint(16, 1024)
        foo = list(range(16, 16 + length))
        bar = list(foo)
        quux = BenesNetwork(length)
        for _ in range(0, 32):
            random.shuffle(foo)
            random.shuffle(bar)
            quux.reset()
            quux.set_switches(foo, bar)

            baz = quux.route(foo)
            assert baz == bar
