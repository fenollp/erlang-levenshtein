# levenshtein

Experiments after reading on https://github.com/rschlaikjer/erlang-levenshtein

```erlang
1> levenshtein:perftest(1000000, fun levenshtein:d2/2).

#Fun<levenshtein.d2.2>: 5036706.940964451 cps
5036706.940964451
```

```erlang
0 erlang-levenshtein.git master (20.3) ∀ r as test shell
===> Verifying dependencies...
===> Compiling levenshtein
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> levenshtein:perftest(1000, fun levenshtein:d0/2).

#Fun<levenshtein.d0.2>: 272.2647640484046 cps
272.2647640484046
2> levenshtein:perftest(1000, fun levenshtein:d1/2).

#Fun<levenshtein.d1.2>: 357.2412891111077 cps
357.2412891111077
3> levenshtein:perftest(1000, fun levenshtein:d2/2).

#Fun<levenshtein.d2.2>: 190318.42556417992 cps
190318.42556417992
4> levenshtein:perftest(1000, fun levenshtein:d2/2).

#Fun<levenshtein.d2.2>: 723258.2494835936 cps
723258.2494835936
5> levenshtein:perftest(1000, fun levenshtein:d2/2).

#Fun<levenshtein.d2.2>: 770069.5526819982 cps
770069.5526819982
```
