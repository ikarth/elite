(ns ijk.elite
  (:require
   [ijk.elite-grammar :as egrammar]
   [ijk.elite-utility :as utility]
   [datascript.core :as d]
   [clojure.spec.alpha :as spec]
   ;;[clojure.edn :as edn]
   [clojure.string :as cstring]
   [clojure.math]
   [clojure.math.combinatorics :as combo]
   [cljs.pprint :as pprint]
   [clojure.set :refer [union]]
   ["js-xxhash" :as xx :refer (xxHash32)]
   [rand-cljc.core :as prng]
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The seed for the orignal game, chosen after searching through many possibilities...
(def elite-seed (utility/make-seed [0x5A4A 0x0248 0xB753]))

;; test twisting the seed
(def planet-two (-> elite-seed
                    utility/twist-seed
                    utility/twist-seed
                    utility/twist-seed
                    utility/twist-seed))

;; (seed-as-int elite-seed)
;; => 81751615624119

;; (def hashed-prng
;;   (xx/xxHash32 "Elite"))


(def completely-random ;from random.org
  [59
147
75
38
189
71
191
93
103
227
12
246
46
144
236
16
53
134
52
214
229
34
224
23
97
224
65
138
241
104
74
235
18
151
97
141
123
148
161
48
138
82
15
190
222
223
46
158
74
144
222
43
26
162
70
2
192
51
244
14
254
102
29
34
35
31
240
184
119
21
225
102
15
163
84
26
232
59
216
70
149
32
99
3
129
239
2
22
5
175
137
251
161
112
217
229
230
84
184
51
157
34
44
84
165
210
23
214
234
44
170
103
162
120
65
152
180
0
15
132
94
12
191
117
245
171
53
234
197
24
140
19
23
152
148
206
10
59
111
166
8
69
145
174
123
184
156
145
222
157
219
50
74
100
254
84
8
68
75
198
44
209
86
126
3
147
60
218
205
206
138
37
142
15
44
113
219
92
129
164
144
165
197
184
95
32
107
254
131
7
134
124
43
62
112
17
214
49
174
43
185
122
112
56
254
38
64
43
121
110
110
82
5
44
114
156
111
154
18
231
193
206
97
72
83
77
58
235
105
230
88
146
55
83
14
213
237
129
138
101
252
234
178
180
202
110
207
226
168
82
4
184
117
194
0
25
2
128
174
132
144
122
162
135
0
188
37
202
155
128
142
170
112
109
84
150
239
14
103
141
89
179
173
67
5
20
198
81
122
33
240
15
21
206
127
55
102
78
230
101
180
49
166
238
145
161
93
221
116
83
135
237
96
40
80
140
244
178
27
121
54
38
84
246
159
195
186
85
74
101
60
172
104
8
162
68
62
200
125
130
43
113
216
33
104
17
12
132
187
247
137
199
221
196
180
95
52
41
91
30
168
194
91
66
22
251
104
199
206
234
64
140
136
196
71
68
207
122
121
235
176
42
239
103
49
223
104
149
142
238
136
210
227
37
68
242
190
21
162
47
141
30
11
8
13
205
67
220
199
245
3
189
216
144
244
7
111
201
54
18
115
95
100
50
98
191
241
124
136
3
138
62
209
241
133
40
18
97
231
162
119
67
156
119
180
237
99
216
53
24
129
228
254
157
91
163
64
79
21
135
236
160
227
234
8
83
67
205
107
154
217
204
103
135
122
222
8
104
201
112
50
173
179
220
56
91
219
184
191
39
238
77
51
185
233
187
139
113
102
191
239
170
92
131
82
2
22
133
193
216
150
213
250
181
114
161
146
106
216
142
196
130
8
155
203
15
77
14
232
225
14
182
147
187
192
170
76
112
160
214
161
100
196
207
173
28
249
6
193
213
54
141
40
43
198
40
46
20
200
208
195
132
20
216
90
185
30
160
245
162
5
177
12
104
185
169
243
106
106
254
78
100
177
242
53
131
193
126
252
87
73
75
242
228
6
255
191
26
78
51
223
205
96
171
201
202
108
2
225
163
207
50
244
135
19
98
127
212
207
13
231
52
219
8
237
18
70
61
123
6
77
114
147
3
121
245
127
64
7
197
151
41
232
200
22
109
49
199
72
0
6
148
215
204
81
144
197
68
222
8
228
113
250
230
181
232
177
14
129
30
122
51
37
35
113
165
183
60
150
69
222
101
247
170
208
229
111
85
159
5
229
210
47
68
67
35
213
169
219
166
94
246
63
32
30
166
2
111
137
143
38
186
137
245
57
229
39
51
183
103
26
87
177
195
147
155
104
216
35
222
233
154
195
55
109
249
39
39
43
238
167
173
36
90
147
179
186
156
17
197
53
32
215
195
3
126
188
149
220
130
219
117
1
13
144
30
151
216
233
123
147
234
138
151
165
5
48
46
14
75
148
146
11
60
42
3
142
96
47
229
192
154
94
103
58
248
158
140
110
247
113
79
240
249
211
121
246
100
17
62
162
63
117
86
144
174
147
70
73
245
215
64
72
231
190
69
11
109
119
237
253
212
217
207
200
153
243
246
53
255
85
139
36
61
244
218
118
65
133
22
229
208
120
139
47
41
159
182
253
114
11
173
173
48
29
173
189
117
209
120
3
207
10
87
167
202
246
248
187
132
250
185
60
198
136
128
96
150
104
131
113
14
142
18
29
91
28
81
185
164
89
91
33
52
48
173
234
204
166
170
67
91
248
124
174
21
39
137
28
188
113
177
242
241
72
214
163
24
210
140
107
65
218
43
15
116
236
232
72
142
203
6
30
187
168
131
88
181
231
160
136
150
4
187
128
67
32
2
55
50
201
122
90
129
2
8
244
171
204
113
180
211
112
110
84
9
36
63
189
12
188
170
38
244
82
73
85
251
254
41
94
167
9
16
131
98
168
248
240
157
111
182
118
61
61
74
155
18
0
13
178
8
185
204
191
220
160
234
31
73
156
43
94
213
128
120
24
209
147
108
128
79
90
53
32
31
4
100
18
68
127
52
156
208
153
146
20
16
35
130
119
250
142
213
174
191
69
238
25
119
58
151
146
227
12
103
136
26
117
248
11
233
88
208
113
83
116
54
56
90
211
81
132
175
194
155
144
220
65
75
118
136
213
146
11
116
241
152
40
144
29
254
84
193
245
172
101
176
112
1
45
103
251
170
177
112
227
160
93
74
68
249
192
124
189
49
193
148
31
15
237
195
255
10
222
251
184
131
200
36
56
149
112
128
40
80
123
229
138
120
219
111
60
67
182
14
254
17
93
45
88
154
164
9
129
216
114
90
171
85
76
142
213
42
19
93
125
185
162
108
194
47
209
123
153
168
46
225
188
225
87
116
197
173
206
226
42
29
17
80
205
251
60
184
29
250
121
66
140
28
27
140
230
20
111
27
180
78
10
165
99
30
101
249
202
122
147
176
156
2
119
55
89
175
244
48
157
115
216
255
49
214
144
152
52
145
249
154
140
67
119
80
254
27
68
20
132
35
108
79
16
249
222
198
28
95
92
154
219
231
40
172
123
236
57
200
137
254
241
15
196
91
67
191
2
162
198
166
162
198
37
26
156
64
204
41
62
129
235
209
13
124
211
141
68
96
31
229
179
60
32
96
147
60
57
115
183
72
212
175
175
183
41
170
225
154
90
127
28
62
139
173
89
208
222
213
16
111
5
21
200
138
48
193
12
250
160
175
217
212
26
203
33
52
64
234
212
118
2
37
144
200
97
19
32
132
201
65
50
156
84
253
1
229
250
26
67
113
234
76
72
225
70
133
176
127
176
104
38
149
145
107
11
174
33
248
176
2
84
239
76
55
208
162
125
240
7
139
244
37
66
11
233
124
175
50
143
11
52
33
41
237
73
49
190
110
127
99
196
74
170
6
250
133
25
242
226
21
50
36
152
252
210
150
99
84
167
252
72
197
102
215
21
127
211
64
40
108
81
131
60
66
81
199
83
24
80
128
108
122
194
245
138
78
232
107
74
205
57
20
75
159
80
156
62
119
122
129
210
67
123
62
236
247
12
153
72
158
255
173
26
119
150
115
59
49
75
252
186
226
14
137
181
202
23
140
74
170
69
96
215
182
236
217
172
94
175
86
47
63
231
121
108
12
160
59
19
71
162
95
129
156
84
131
132
16
157
219
173
174
166
90
94
84
170
21
70
222
28
6
220
233
105
248
52
113
90
53
191
94
38
139
194
71
90
145
35
201
230
175
82
120
102
239
175
41
182
19
104
11
70
231
199
199
71
182
186
209
7
31
15
162
45
99
40
6
107
140
182
154
227
73
188
159
243
46
40
184
137
209
43
123
133
253
234
148
162
206
108
87
196
133
151
90
119
8
34
68
9
136
88
57
136
11
190
225
125
240
231
38
74
49
56
131
138
242
205
152
173
97
112
82
64
24
131
111
42
232
98
153
153
87
43
104
75
128
178
102
137
13
100
172
135
162
219
23
218
97
116
251
122
221
138
89
12
162
102
102
142
218
90
235
216
7
203
117
128
197
206
48
250
209
206
163
31
216
232
146
254
159
46
205
21
246
10
40
176
92
25
219
220
27
249
119
242
155
10
190
78
26
246
153
55
59
69
251
69
81
106
255
85
2
68
58
64
175
143
252
66
5
168
166
68
15
94
90
108
138
165
12
150
15
231
242
150
155
140
96
231
102
197
22
33
25
123
227
72
218
8
13
52
155
222
222
27
215
157
57
10
132
194
139
168
29
23
234
128
26
213
30
154
190
165
18
222
95
174
229
171
111
219
169
198
234
26
181
253
171
15
90
46
7
232
21
213
71
177
166
170
214
24
173
122
66
59
29
205
138
116
132
67
158
166
232
88
185
138
243
161
29
134
30
201
254
154
241
10
251
236
20
123
178
33
162
96
46
66
243
224
119
132
16
135
51
27
249
132
81
232
32
109
15
29
224
83
66
202
130
74
174
201
147
128
135
200
97
77
179
146
11
186
93
191
136
81
173
62
166
112
213
169
247
147
28
57
93
242
158
112
109
176
115
91
192
31
202
78
116
3
235
174
144
122
5
242
165
102
195
57
132
108
230
221
15
100
253
194
8
36
218
14
243
238
169
96
193
146
239
215
131
200
100
21
40
80
21
32
218
93
66
238
196
159
84
98
169
133
157
194
38
12
115
209
11
218
60
227
77
101
52
183
167
50
221
211
144
154
143
215
82
149
158
238
116
181
36
243
197
195
105
165
151
163
144
83
240
66
90
100
40
254
35
170
165
239
246
99
9
129
242
164
214
159
121
4
40
151
227
240
253
68
249
86
1
55
5
180
29
162
252
77
81
185
148
95
55
67
114
108
27
54
175
64
197
212
244
79
6
32
68
76
113
165
210
238
110
95
185
56
118
222
155
45
201
39
74
61
195
193
25
22
221
146
73
46
101
219
225
15
61
180
40
28
28
193
152
42
224
100
202
58
122
226
156
77
61
106
156
93
209
9
88
80
219
105
40
22
253
152
27
226
192
168
16
78
111
48
7
135
156
107
48
201
30
121
12
222
205
5
110
209
56
49
202
34
173
188
75
96
246
254
6
227
196
250
88
206
64
227
6
49
157
17
116
178
14
37
36
19
56
152
89
75
195
10
62
152
178
17
218
232
59
203
15
28
117
133
73
37
70
182
53
82
39
199
49
214
251
112
119
38
58
12
223
241
146
123
14
122
120
17
142
154
95
119
239
115
105
114
7
118
79
173
18
16
77
182
7
96
100
194
191
208
179
105
117
104
55
162
105
248
145
139
24
45
77
157
223
138
20
59
170
109
59
103
123
129
92
162
171
38
207
199
155
186
235
149
220
248
222
36
81
134
152
228
155
148
169
96
226
177
5
31
35
55
95
61
147
152
18
105
210
192
156
145
162
19
147
97
71
78
122
57
131
93
202
11
235
234
221
15
245
17
36
190
136
54
38
141
111
187
144
68
199
63
178
139
13
151
79
11
251
198
43
187
195
246
182
240
83
243
171
95
96
21
20
68
95
151
109
179
220
153
254
51
207
52
3
172
114
0
167
201
50
134
206
180
84
53
125
96
36
34
72
144
227
83
106
233
121
197
228
253
137
91
170
120
53
97
61
79
215
233
237
103
230
77
103
21
64
128
192
198
38
32
240
153
232
156
179
123
37
34
163
238
57
15
71
29
153
54
181
63
221
191
226
204
149
65
109
96
145
109
24
29
174
217
182
139
204
227
182
222
128
66
24
192
239
146
227
77
139
155
101
14
228
168
231
160
165
22
51
166
46
226
13
32
14
71
44
85
47
174
203
233
84
34
100
69
224
96
69
78
90
147
188
117
103
90
198
91
135
159
232
42
48
68
47
136
17
212
51
107
251
0
45
93
124
133
106
199
112
255
118
238
115
54
104
24
253
92
60
61
178
74
119
99
73
14
238
116
248
234
19
247
113
112
130
250
11
201
138
118
229
21
148
200
225
27
24
116
89
32
16
136
150
44
138
122
142
131
144
221
46
58
98
40
215
20
226
51
54
76
192
61
130
112
230
223
217
21
159
211
46
20
79
144
187
14
36
161
169
219
15
156
169
81
171
68
199
16
95
87
242
170
2
112
26
184
162
216
116
95
55
13
254
227
7
14
233
199
11
241
82
223
14
179
59
74
106
92
25
115
19
19
37
92
82
230
194
119
159
122
155
87
109
31
81
130
226
246
89
110
254
27
34
183
156
28
38
65
52
188
76
80
98
175
7
202
139
151
147
180
170
123
36
141
13
37
249
61
219
40
170
60
245
32
144
131
20
76
235
138
47
20
27
176
173
190
56
6
225
116
194
113
53
210
153
21
243
56
218
119
68
95
109
188
87
232
30
12
191
74
98
142
104
10
217
168
205
238
244
103
85
163
44
145
126
2
97
83
82
186
160
191
100
181
30
42
156
26
53
89
55
10
69
33
17
85
188
96
124
168
236
0
150
79
174
175
39
3
54
114
169
246
46
159
80
203
98
119
234
119
80
233
76
237
69
145
156
29
0
248
121
78
18
93
204
61
193
23
226
49
215
64
194
82
73
209
248
107
213
31
40
90
219
210
134
235
231
184
86
45
114
157
176
141
171
226
39
177
122
217
14
141
238
220
134
245
145
0
159
221
126
135
44
215
108
128
206
27
88
215
42
59
167
135
1
111
140
93
149
215
223
222
125
104
170
61
135
180
248
194
132
49
16
87
14
100
240
6
61
76
92
202
132
37
92
174
184
107
247
197
191
4
185
76
163
2
157
210
139
70
131
1
42
218
71
175
166
38
80
102
91
236
244
81
177
45
154
72
235
168
238
209
32
255
137
251
139
156
210
107
83
1
181
130
183
159
92
206
118
219
225
182
55
137
32
225
116
140
241
227
125
26
247
192
57
159
77
76
6
190
111
217
233
36
146
222
252
116
117
129
106
135
186
171
16
227
255
64
240
95
42
133
237
41
113
121
74
238
15
235
81
126
240
30
128
220
236
79
38
230
11
188
131
10
112
123
208
41
147
180
105
160
103
74
77
167
244
166
89
109
175
172
102
168
171
37
214
154
48
107
55
116
37
153
240
148
147
157
113
46
202
162
127
159
218
114
238
99
178
31
19
175
131
144
237
221
170
9
140
41
222
36
244
101
39
131
235
232
176
147
179
224
19
61
120
192
206
21
117
87
165
223
4
21
93
53
109
122
151
233
240
146
223
113
140
5
86
13
135
152
137
138
104
88
254
21
93
216
77
87
43
184
43
179
60
125
23
79
91
154
37
1
57
78
77
95
193
15
191
149
114
94
190
238
77
78
166
204
189
66
249
60
64
24
30
163
241
207
126
19
183
135
231
158
153
32
31
80
127
148
29
106
82
129
190
156
128
172
119
200
93
136
74
36
221
32
128
237
46
110
190
52
100
234
215
11
182
240
60
160
140
209
73
13
37
173
193
6
104
246
162
192
170
226
110
125
142
0
41
65
228
220
253
210
12
75
42
8
55
68
131
156
201
212
62
149
253
189
128
49
111
6
40
115
102
204
119
102
249
252
106
110
78
128
235
233
243
134
36
34
1
212
251
131
247
143
23
6
194
33
236
52
38
10
128
147
249
122
62
164
67
44
20
155
238
192
192
210
24
140
115
6
71
86
204
187
241
227
228
100
19
223
129
40
244
207
253
4
33
6
83
231
116
160
18
117
111
228
38
48
121
12
221
37
34
227
57
24
89
245
233
165
40
118
95
60
52
73
159
115
33
234
153
63
120
76
163
44
53
16
147
181
42
138
184
164
44
201
138
245
213
167
51
105
195
16
153
147
196
127
25
210
218
181
7
7
134
95
173
115
68
191
190
30
114
214
105
80
181
168
90
177
122
41
162
187
124
146
140
198
207
222
231
49
254
111
66
128
236
174
107
175
245
87
15
235
182
192
247
163
138
55
207
48
159
168
205
57
204
147
197
84
108
114
116
130
219
4
138
227
165
38
146
96
2
142
245
39
118
164
237
92
101
66
112
17
180
247
50
176
84
142
140
30
24
248
131
151
181
129
248
208
31
127
208
172
247
36
196
238
164
222
69
88
175
11
47
162
7
237
221
139
154
121
101
120
47
214
58
124
117
79
67
127
31
111
23
165
133
92
244
218
15
9
1
160
92
140
237
64
3
255
255
163
110
3
207
115
121
80
95
44
188
43
205
179
110
152
82
40
251
122
29
193
188
188
30
217
137
138
173
103
185
1
140
7
77
137
67
36
37
194
59
66
229
27
214
54
171
76
60
239
127
164
88
35
94
40
38
131
74
129
164
17
194
14
112
56
226
204
51
75
52
32
226
162
13
74
14
35
194
192
242
37
117
13
135
142
103
46
139
218
237
31
74
54
55
31
226
45
243
62
87
122
211
52
210
99
211
212
88
232
39
147
120
179
210
52
1
20
42
46
42
228
253
231
210
8
121
46
18
190
254
60
46
196
59
156
48
145
69
39
36
203
86
114
73
247
122
35
183
104
93
7
68
19
194
4
161
153
86
105
153
6
128
66
224
215
128
128
197
32
19
189
130
50
149
222
123
127
197
199
141
198
16
66
246
5
103
87
16
82
145
12
213
119
227
49
52
145
166
119
15
65
130
157
56
220
136
91
212
87
255
165
50
29
37
2
26
231
179
13
42
206
145
70
173
64
229
51
137
8
24
52
90
229
175
19
221
136
158
13
145
143
133
249
118
188
232
131
169
206
29
93
50
25
127
134
56
113
84
69
254
212
85
57
149
193
145
32
90
231
7
87
205
209
247
227
19
80
124
21
92
218
19
75
156
214
117
161
213
145
92
99
161
119
67
240
150
47
101
33
19
25
100
128
234
249
247
227
223
98
160
60
176
200
177
67
117
215
41
109
34
52
191
120
24
63
57
88
200
137
228
25
12
234
255
37
167
74
161
126
133
210
227
129
38
233
46
3
163
23
36
32
151
32
247
158
177
173
41
227
33
103
208
209
138
163
181
110
249
20
62
159
20
103
200
217
61
225
26
68
191
94
41
21
17
81
219
50
31
237
182
45
61
135
211
52
242
75
88
96
235
194
211
206
9
238
213
146
11
39
92
9
197
86
33
160
36
43
110
125
36
117
48
23
101
148
205
10
80
155
80
129
22
230
42
131
77
247
123
13
97
82
35
172
165
150
211
18
177
21
92
138
215
182
57
150
70
70
203
167
254
170
119
116
59
179
221
165
21
129
249
105
86
32
85
221
166
250
56
229
171
137
109
54
239
202
20
32
122
57
52
87
234
49
120
197
212
138
98
81
27
43
64
7
140
145
48
179
133
101
56
25
243
64
48
142
203
173
255
111
171
227
106
39
179
106
215
123
236
10
146
65
154
59
33
97
135
223
131
86
122
84
39
182
149
53
171
253
215
62
230
253
234
210
18
91
124
194
110
55
160]
  )

;; test getting all of the planets
(defn planet-seed-list
  ([] (planet-seed-list elite-seed))
  ([seed]
   (reduce (fn [current next-id] (concat current [(utility/twist-to-next-planet (last current))]))
           [seed]
           (range 255))))


(spec/def :seed/specifier
  (spec/and
   (spec/coll-of number? :kind vector? :count 3 :into [])
   ;(spec/every #(>= 0 % 255) :count 6)
   ))


(def elite-schema {:seed/planet            {:db/cardinality :db.cardinality/one   :db/unique :db.unique/identity}
                   :seed/description       {:db/cardinality :db.cardinality/one}
                   :seed/galaxy            {:db/cardinality :db.cardinality/one}
                   :planet/economy-type    {:db/cardinality :db.cardinality/one}
                   :planet/species         {:db/cardinality :db.cardinality/one}
                   :planet/government-type {:db/cardinality :db.cardinality/one}
                   :planet/name-length     {:db/cardinality :db.cardinality/one}
                   :planet/partial-name    {:db/cardinality :db.cardinality/one}
                   :planet/name            {:db/cardinality :db.cardinality/one}
                   :planet/description     {:db/cardinality :db.cardinality/one}})


;; .QQ16 in the original code.
;; I'm storing them from 0-31 here instead of the original 128 to 159,
;; because on a modern system I don't need to cram it into the same memory
;; as the other text glyphs and commands so we save an extra step.
(def elite-planet-name-digraphs
  [
 ""                ;; for planet names, 0 is a special case. I'm encoding this in this table instead of in the function.   ;;"AL"              ; Token 128
 "LE"              ; Token 129
 "XE"              ; Token 130
 "GE"              ; Token 131
 "ZA"              ; Token 132
 "CE"              ; Token 133
 "BI"              ; Token 134
 "SO"              ; Token 135
 "US"              ; Token 136
 "ES"              ; Token 137
 "AR"              ; Token 138
 "MA"              ; Token 139
 "IN"              ; Token 140
 "DI"              ; Token 141
 "RE"              ; Token 142
 "A"               ; Token 143, originally "A?"
 "ER"              ; Token 144
 "AT"              ; Token 145
 "EN"              ; Token 146
 "BE"              ; Token 147
 "RA"              ; Token 148
 "LA"              ; Token 149
 "VE"              ; Token 150
 "TI"              ; Token 151
 "ED"              ; Token 152
 "OR"              ; Token 153
 "QU"              ; Token 154
 "AN"              ; Token 155
 "TE"              ; Token 156
 "IS"              ; Token 157
 "RI"              ; Token 158
 "ON"              ; Token 159
   ])


(defn extract-bits
  "Utility function to streamline extracting specific parts of the seed.
  Takes seed, the byte, the starting bit, and how many total bits."
  [seed word start amount]
  (utility/bin-to-byte (utility/get-seed-bits seed (word utility/elite-index) start amount)))


(defn extract-seed-for-name-length
  "Given a planet seed, determine the length of the name (3 or 4 digraphs)"
  [seed]
  (if (= 1 (utility/get-value-from-seed seed (:s0_lo utility/elite-index) 1 1))
    4
    3))


(defn generate-name-start [seed]
  (let [token-seed seed
        name-length-remaining (extract-seed-for-name-length seed)
        planet-name ""
        ]
    [token-seed name-length-remaining planet-name]))

(defn op-generate-name-start [seed]
  (zipmap [:seed/token-seed
           :number/name-length-remaining
           :string/planet-name-partial]
          (generate-name-start seed)))

;; Ways we could design this DSL:
;;
;; * We could have the op functions explicitly map the outputs to datalog keys.
;;      - Needs to specify for every function
;;      - Most functions just need a straightforward list, so might implement something to auto-assemble a wrapper function.
;; * For inputs, we could consume everything (removing it from the blackboard) and only keep the ones that get output again
;;      - Seems a bit wasteful
;;      - have to remember to specify all of the non-consumed terms all of the time
;;      + relatively straightforward
;; * We could add some kind of indicator to flag stuff that gets consumed
;;      - Seems like it complicates the grammar a lot
;; * We could indicate stuff getting consumed as part of the output
;; * We could ignore the problem for the moment.



(defn generate-name
  "Generate the planet name recursively, using the elite-planet-name-diagraphs table as the source data.
  
  Having this operation be recursive across multiple calls follows the original BBC Elite, plus it lets us demonstrate how the generator can handle partially-constructed artifacts."
  [input]
  (if (string? input)
    input
    (let [[seed-token name-length-remaining name-in-progress] input]
      (if (< name-length-remaining 1)
        name-in-progress
        (let [new-token (utility/twist-seed seed-token)
              index-bits (utility/get-seed-bits seed-token
                                        (:s2_hi utility/elite-index) 3 5)
              digraph-index (utility/bin-to-byte index-bits)
              digraph (get elite-planet-name-digraphs digraph-index)]
          [new-token
           (dec name-length-remaining)
           (str name-in-progress digraph)
           ])))))

(galactic-x elite-seed)
(extract-bits elite-seed :s1_hi 0 8) 

(defn galactic-coordinates [seed]
  (let [galactic-x (extract-bits seed :s1_hi 0 7)
        galactic-y (/ (extract-bits seed :s0_hi 0 7) 2)
        size-on-map (utility/bin-to-byte (mapv max [0 1 0 1 0 0 0 0] 
                                         (utility/get-seed-bits seed (:s2_lo utility/elite-index) 0 7)))]
    {:planet/galactic-x  galactic-x
     :planet/galactic-y  galactic-y
     :planet/size-on-map size-on-map}))

(defn galactic-x [seed]
  (extract-bits seed :s1_hi 0 8))

(defn galactic-y [seed]
  (extract-bits seed :s0_hi 0 8))

(defn galactic-x-uniform-random [seed galaxy-index planet-index]
  ;;(let [hash-gen (.h32 js/xx)])
  (let [composite-index (+ 2048 (* galaxy-index 256) planet-index)]
    (nth completely-random composite-index))
  )

(galactic-x-uniform-random 1 0 1)

(defn galactic-y-uniform-random [seed galaxy-index planet-index]
  (let [composite-index (+ 0 (* galaxy-index 256) planet-index)]
        (nth completely-random composite-index)))

;; (let [hash-gen )]
;;   hash-gen)
  

(defn size-on-map [seed]
  (utility/bin-to-byte (mapv max [0 1 0 1 0 0 0 0] (utility/get-seed-bits seed (:s2_lo utility/elite-index) 0 8))))

(defn planet-government
  "Planet government is a number from 0 to 7, extracted directly from the bits in the seed.

  The first operation in the original code, and the most basic."  
  [planet-seed]
  (extract-bits planet-seed :s1_lo 2 3))


(defn planet-economy
  "Generate the economic level and type of the planet.
  Note that we need the governement type, because anarchy and feudal governements are restricted from being rich.
  We *could* just calculate the government type on the fly, since in the original algorithm this was all one dense code block that fed into the next step, but for the purposes of our more flexible generator, I'm opting to make it a parameter - maybe there's some new operation that alters the government type or something, so we'll want the final value instead of a parallel calculation. But in other circumstances we might opt for the parallel calculation."
  [planet-seed planet-government]
  (let [eco-base
        (try 
          (utility/get-seed-bits planet-seed (:s0_hi utility/elite-index) 5 3)
          (catch js/Error e
            (println "An economic error occurred:" e)
              0))
        adjusted (assoc eco-base 1 (if (< planet-government 2) 1 (nth eco-base 1)))
        type (nth eco-base 0)
        prosperity (try
                     (utility/bin-to-byte adjusted)
                     (catch js/Error e
                       (println "An economic error occurred: " e)
                       0
                       ))
        ;; Returning both the prosperity-coerced-to-an-integer *and* the flipped number
        ;; is an example of how the original code compactly reuses its computational
        ;; resources, in contrast with how our more-modular reconstruction needs to
        ;; separate the values. I could avoid this if I just kept it as the bytes
        ;; (or if I had a byte-flipping function)
        ;;flipped-economy (bin-to-byte (invert-bits adjusted))
        ;; ...on second thought, I wrote the byte-flipping function.
        ]
    ;;(println [eco-base adjusted prosperity])
    [type prosperity]))


(defn planet-tech-level
  "The tech level formula is:
       flipped_economy + (s1_hi AND %11) + (government / 2)" 
  [planet-seed economy government]
  (+ (utility/invert-byte (second economy) 3)
     (extract-bits planet-seed :s1_hi 6 2)
     (clojure.math/ceil (/ government 2))))


(defn planet-tech-level-from-prosperity
  "The tech level formula is:
       flipped_economy + (s1_hi AND %11) + (government / 2)

  There are two of these functions because the ecological query was
  easier to write if it used prosperity directly." 
  [planet-seed prosperity government]
  (+ (utility/invert-byte prosperity 3)
     (extract-bits planet-seed :s1_hi 6 2)
     (clojure.math/ceil (/ government 2))))


;; 2022/9/6 - There is a bug in the species generation. For example, 252 Tiinlebi is
;;            reporting "Green Horned Felines" when it should be "Green Horned Humanoids"
;; 2022/9/6 - The bug was in this table: "Lobster" was missing from the list.

(def species-table
  [["Large ", "Fierce ", "Small ", "", "", "", "", ""]
  ["Green " "Red " "Yellow" "Blue " "Black " "Harmless " "" "" "" ""]
  ["Slimy " "Bug-eyed " "Horned " "Bony " "Fat ", "Furry ", "", ""]
  ["Rodents"
   "Frogs"
   "Lizards"
   "Lobsters"
   "Birds"
   "Humanoids"
   "Felines"
   "Insects"]
   ])



(defn planet-goat-soup
  "Generate the 'goat soup' planet description string"
  [seed system-name]
  (egrammar/generate-goat-soup seed system-name))

(defn planet-species
  "Generate a (brief) description of the local planetary species."
  [seed]
   ;; Check bit 7 of s2_lo - if it is zero return "Human Colonials"
  (if (= 0 
         (first (utility/get-seed-bits seed (:s2_lo utility/elite-index) 0 1)))
    "Human Colonials"
    (let [;; A bunch of fiddly bitmapping operations. Remember that our subvec indexing is counting from the opposite direction as the notes on the BBC Elite site...
          ;; register-A = s2_hi
          register-A 
          (vec (concat [0 0]
                       (into [] (utility/get-seed-bits seed (:s2_hi utility/elite-index) 0 6))))
          ;; bits 2-4 of A, by using a mask which multiplies the bit strings together 
          species-size  (utility/bin-to-byte (mapv * [0 0 0 0 0 1 1 1] register-A))
          ;; bits 5-7 of A, by just grabbing the subvec 
          species-color (subvec register-A 3 5)
          ;; A = bits 0-2 of (s0_hi EOR s1_hi)
          register-A-new
          (mapv (fn [a b] (if (not= a b) 1 0))
                (utility/get-seed-bits seed (:s0_hi utility/elite-index) 0 8)
                (utility/get-seed-bits seed (:s1_hi utility/elite-index) 0 8))
          ;; texture = bits 0-2 of the new A
          texture (mapv * [0 0 0 0 0 1 1 1]
                        register-A-new)
          ;; bits 0-1 of s2_hi
          intermediate-B (mapv * [0 0 0 0 0 0 1 1]
                           (utility/get-seed-bits seed (:s2_hi utility/elite-index) 0 8))
          ;; add register B to A-new
          intermediate-B-plus (utility/bitwise-add-vec texture
                                           (mapv * [0 0 0 0 0 0 1 1]
                                                 intermediate-B))
          ;; take bits 0-2 of B-plus
          species-name (mapv * [0 0 0 0 0 1 1 1]
                             intermediate-B-plus)         
          species-type (utility/bin-to-byte (utility/get-seed-bits seed (:s2_hi utility/elite-index) 6 2))
          species-id  [(utility/bin-to-byte (utility/get-seed-bits seed (:s2_hi utility/elite-index) 3 3)) ;; size
                       (utility/bin-to-byte (utility/get-seed-bits seed (:s2_hi utility/elite-index) 0 3)) ;; color
                       (utility/bin-to-byte (subvec texture 5)) ;; texture                  
                       (utility/bin-to-byte species-name)]] ;; type
      (apply str 
       (map #(get %2 %1) species-id species-table)))))


(defn planet-population-size [tech-level economy government]
  (let [econ-prosperity ;; econ can be passed in as a tuple of [type, prosperity]
        (if (or (vector? economy)
                (seq? economy))
          (second economy)
          economy)]
  (+
   (* tech-level 4)
   econ-prosperity
   government
   1)))

(defn planet-productivity [economy government population]
  (let [econ-prosperity ;; econ can be passed in as a tuple of [type, prosperity]
        (if (or (vector? economy)(seq? economy))
          (second economy)
          economy)]
  (* 8
     (+ (utility/invert-byte econ-prosperity 3) 3)
     (+ government 4)
     population)))

(defn government-name [gov-type]
  (nth [:anarchy :feudal :multi-government :dictatorship :communist :confederacy :democracy :corporate-state]
       gov-type
       :unknown))

(defn economy-name [[type prosperity]]
  [(get {0 :rich
         1 :average
         2 :poor
         3 :mainly
         4 :mainly
         5 :rich
         6 :average
         7 :poor}
        prosperity)
   (nth [:industrial :agricultural] type)])


(defn list-reachable-systems
  "Given an indexed list of planet coordinates, return a list of planets within jump range."
  [system-coordinates list-of-planet-coordinates jump-range]
  (filterv (fn [p] (and (< 0 (second p)) (<= (second p) jump-range)))
           (mapv (fn [p index] [(first p)
                                (utility/distance-2d-bbc-elite system-coordinates (second p))
                                ])
                 list-of-planet-coordinates
                 (range))))


(defn calculate-hub-count
  "Given a list of x,y galactic coordinates, figure out how many systems are in jump range of this system"
  [system-coordinates list-of-planet-coordinates jump-range]
  (count (list-reachable-systems system-coordinates list-of-planet-coordinates jump-range)))


(def commodities
  ;; Base price, economic factor, unit, base quantity, mask, keyword
  [[19,  -2, 't',   6, 2r00000001  :food          :legal]
   [20,  -1, 't',  10, 2r00000011  :textiles      :legal]
   [65,  -3, 't',   2, 2r00000111  :radioactives  :legal]
   [40,  -5, 't', 226, 2r00011111  :slaves        :illegal] ;; illegal
   [83,  -5, 't', 251, 2r00001111  :liquor        :legal]
   [196,  8, 't',  54, 2r00000011  :luxuries      :legal]
   [235, 29, 't',   8, 2r01111000  :narcotics     :illegal] ;; illegal
   [154, 14, 't',  56, 2r00000011  :computers     :legal]
   [117,  6, 't',  40, 2r00000111  :machinery     :legal]
   [78,   1, 't',  17, 2r00011111  :alloys        :legal]
   [124, 13, 't',  29, 2r00000111  :firearms      :illegal]
   [176, -9, 't', 220, 2r00111111  :furs          :legal]
   [32,  -1, 't',  53, 2r00000011  :minerals      :legal]
   [97,  -1, 'k',  66, 2r00000111  :gold          :legal]
   [171, -2, 'k',  55, 2r00011111  :platinum      :legal]
   [45,  -1, 'g', 250, 2r00001111  :gem-stones    :legal]
   [53,  15, 't', 192, 2r00000111  :alien-items   :unavailable]
   ])

(defn commodity-price [item-number]
  
  )


(defn calculate-market-price [commodity-number economy visit-seed]
  (let [commodity (apply hash-map (interleave [:base-price :economic-factor :unit :quantity :mask :key] (nth commodities commodity-number)))
        base-price (+ (:base-price commodity) (bit-and visit-seed (:mask commodity)))
        econ-factor (* economy (abs (:economic-factor commodity)))
        final-price (* (+ base-price econ-factor) 4)]
    final-price))

(defn calculate-market-available [commodity-number economy visit-seed]
  (let [commodity (apply hash-map (interleave [:base-price :economic-factor :unit :quantity :mask :key] (nth commodities commodity-number)))
        base-amount (+ (:quantity commodity) (bit-and visit-seed (:mask commodity)))
        econ-factor (* economy (abs (:economic-factor commodity)))
        final-amount (rem (- base-amount econ-factor) 64)]
    (max 0 final-amount)))

(defn calculate-trade-value [commodity-number planet-one-econ planet-two-econ]
  (let [ship-hold-size
        (if (= 't' (nth (nth commodities commodity-number) 2))
          35 ;; Cobra Mk III cargo hold, with cargo bay extention = 35 tons
          65535)
        legal-status (nth (nth commodities commodity-number) 6)
        legal-multiplier
        (if (= :unavailable legal-status)
          0          
          (if (= :illegal legal-status
                 )
            0.1
            1.0
            ))]
    ;;(println legal-multiplier "\t "(nth (nth commodities commodity-number) 5))
     (let [low
           (* legal-multiplier
              (* (min ship-hold-size (calculate-market-available commodity-number planet-two-econ 0xFF))
                 (- (calculate-market-price commodity-number planet-one-econ 0xFF)
                    (calculate-market-price commodity-number planet-two-econ 0x00))))
           high
           (* legal-multiplier
              (* (min ship-hold-size (calculate-market-available commodity-number planet-one-econ 0xFF))
                 (- (calculate-market-price commodity-number planet-two-econ 0xFF)
                    (calculate-market-price commodity-number planet-one-econ 0x00))))]
       [(max low high) (min low high)])))



(> 0 1)

(defn calculate-trade-route-value [planet-one-econ planet-two-econ planet-two-id]
  ;; (map (fn [index] (let [[high-profit low-profit]
  ;;                        (calculate-trade-value index planet-one-econ planet-two-econ)]
  ;;                    {:commodity (nth (index commodities) 5) :low low-profit :high high-profit}))
  ;;      (range (count commodities))
       ;; 0
       
       ;; )
  ;;(println planet-one-econ)
  ;;(println planet-two-econ)
  (reduce
   (fn [current-profit commod]
     {:highest
      (if (> (:profit commod) (:profit current-profit))
        (:commodity commod)
        (:highest current-profit))
      :profit         
      (max (:profit current-profit) (:profit commod))
      :destination planet-two-id
      })
   {:profit 0}
   (mapv
    (fn [c-index]
      (let [comd (nth commodities c-index)
            c-name (nth comd 5)
            [c-high c-low] (calculate-trade-value c-index planet-one-econ planet-two-econ)
            c-profit (- c-high c-low)
            ]
        {:commodity c-name :profit c-profit :high c-high :low c-low}))
    (range (count commodities)))))

;;(first (export-galaxy elite-seed))



(defn round-pop [pop]
  (/ (.round js/Math (* 10 pop)) 10))

(defn test-galaxy-generator []
  (let []
    (println "\n\n\n\n\n\n\n\n\n\n\n\n")
    (doseq [[r p] (map-indexed (fn [index item]
                                 [index item])
                               (planet-seed-list))]      
      (let [planet-coord-list (map-indexed (fn [index p] [index [(galactic-x p) (galactic-y p)]]) (planet-seed-list))
            jump-range 70
            gov (planet-government p)
            econ (planet-economy p (planet-government p))
            tech (planet-tech-level p econ gov)
            pop (planet-population-size tech econ gov)
            prod (planet-productivity econ gov pop)
            name (last (take 7 (iterate generate-name (generate-name-start p))))
            species (planet-species p)
            galactic-x (galactic-x p)
            galactic-y (galactic-y p)
            reachable-systems (list-reachable-systems [galactic-x galactic-y] planet-coord-list jump-range)
            ;;hub-count (calculate-hub-count [galactic-x galactic-y] planet-coord-list jump-range)
            hub-count (count reachable-systems)
            goat-soup-string (egrammar/generate-goat-soup p name)
            ]
        (if true;(< 245 r )
          (println 
           (map 
            #(str %1 ":\t "%2 "\n")
            ["id" "name" "seed" "species" "government" "economy" "tech-level" "pop. size" "productivity" "gal. coords" "neighbors" "hub count" "description"]
            [r
             name
             (map #(. % toString 16) (utility/get-seed-bytes p))
             species
             [gov (government-name gov)]
             [econ (economy-name econ)]
             tech
             [pop (str (round-pop (* pop 0.1)) " billion")]
             prod
             [galactic-x galactic-y]
             reachable-systems
             hub-count
             goat-soup-string
             ])))))))



;;(test-galaxy-generator)

(defn run []
  (.log js/console "Hello")
  (println "World"))

;; (map-indexed (fn [index p]
;;                [index [(galactic-x p) (galactic-y p)]]) planet-seed-list)

;; (mapv planet-government planet-seed-list)
;; (mapv #(planet-economy %1 (planet-government %1)) planet-seed-list)


;; (let [planet-coord-list (mapv)])


;;  (nth planet-seed-list 7)


;; (planet-government (nth planet-seed-list 7))
;; (planet-economy (nth planet-seed-list 7) 3)


(defn export-names [galaxy-seed]
  (let [this-galaxy-planet-list (planet-seed-list galaxy-seed)]
    (for [p this-galaxy-planet-list]
      (let [name (last (take 7 (iterate generate-name (generate-name-start p))))
            description (planet-goat-soup p name)
            ]
        [name description]

        ))))

;;(export-names elite-seed)

(defn export-galaxy [galaxy-seed galaxy-index]
  (let [jump-range 70
        this-galaxy-planet-list (planet-seed-list galaxy-seed)
        indexed-planets
        (map-indexed (fn [i p] [i p]) this-galaxy-planet-list)
        planet-coord-list (map-indexed (fn [index p] [index [(galactic-x p) (galactic-y p)]]) (planet-seed-list galaxy-seed))
        planets-in-galaxy        
        (for [[index p] indexed-planets
              :let [name (last (take 7 (iterate generate-name (generate-name-start p))))
                    gov  (planet-government p)
                    econ (planet-economy p gov)
                    tech (planet-tech-level p econ gov)
                    pop  (planet-population-size tech econ gov)
                    prod (planet-productivity econ gov pop)
                    galactic-x (galactic-x-uniform-random p galaxy-index index)
                    galactic-y (galactic-y-uniform-random p galaxy-index index)
                    reachable-systems (list-reachable-systems [galactic-x galactic-y] planet-coord-list jump-range)
                    hub-count  (count reachable-systems)
                    description (planet-goat-soup p name)
                    trade-routes 
                    (map

                     (fn [other-data]
                       ;;(println this-galaxy-planet-list)
                       ;;(println other-data)
                       (let [other-planet (nth this-galaxy-planet-list (nth other-data 0))
                             other-gov (planet-government other-planet)
                             other-econ (planet-economy other-planet other-gov)
                             ]
                         (calculate-trade-route-value (second econ) (second other-econ) (nth other-data 0))
                         ;;other-data

                         ))
                     reachable-systems)
                    ]]
          {:index      index
           :name       name     
           :species    (planet-species p)
           :government (government-name gov)
           :economy    (economy-name econ)
           :tech-level tech
           :population pop
           :productivity prod
           :location   [galactic-x galactic-y]
           :neighbors  reachable-systems
           :neighbor-count hub-count
           :description description
           :trade-routes trade-routes
           })]
    planets-in-galaxy))

(let [a (first (export-galaxy elite-seed))]
  (if (= a 0) true false)
  )



;; (defn export-planet-names [galaxy-seed]
;;   (let [this-galaxy-planet-list (planet-seed-list galaxy-seed)]
;;     (map (fn [p]
;;            (let [name (last (take 7 (iterate generate-name (generate-name-start p))))
;;                  description ]
             
             
;;              this-galaxy-planet-list)))))

;; (count
;;  (set
;;   (export-planet-names elite-seed)))

;;(export-planet-names elite-seed)


(clj->js (first (export-galaxy elite-seed)))

(comment
  (utility/write-to-file
   (export-galaxy elite-seed)
   "elite-galaxy-01.edn")
  (utility/write-to-file
   (.stringify js/JSON
               (clj->js (export-galaxy elite-seed)))
   "elite-galaxy-01.json"))


(def local-galaxy-seeds
  (take 8
        (iterate utility/galaxy-twist elite-seed)))

;; (mapv utility/get-seed-bytes
;;   local-galaxy-seeds)

(comment
  (for [ [i g] (map-indexed (fn [a b] [a b])
                            local-galaxy-seeds)]
    (let [gal-data (export-galaxy g)]
      (println "Galaxy #" i " : " (utility/get-seed-bytes g))
      (utility/write-to-file gal-data (str "elite-galaxy-trade-0" i ".edn"))
      (utility/write-to-file (.stringify js/JSON (clj->js gal-data)) (str "elite-galaxy-trade-0" i ".json"))
      )))
      
;; (count
;;  (set
;;   (export-planet-names elite-seed)))

;;(range 65535)

;;(make-seed )

;; (union (set [3 4]) (set [3 6]))

;; (combo/count-permutations [ 1 2 3])

;; (range 10)
(comment
  (let [galaxies-count 1
        planet-names-list
        (take (* galaxies-count 256)
              (let [galaxies            
                    (map utility/make-seed (take 15000000000000000 (combo/cartesian-product (range 65536) (range 65536) (range 65536))))]
                (map #(str (utility/get-seed-bytes %)) galaxies);;(map export-planet-names galaxies)        
                ))
        names-set (union (map set planet-names-list))
        names-count (count names-set)
        ]
    (utility/write-to-file (.stringify js/JSON (clj->js planet-names-list)) "planet-seeds.txt")
    
    (utility/write-to-file (str names-count " / " (* 256 galaxies-count) "\n" planet-names-list) "planet-names-count.txt")
    ))

(defn seed-as-int [seed]
  (apply + 
         (map * (map (fn [n] (reduce * (repeat n 256))) (range))
              (reverse
               (utility/get-seed-bytes seed))))) 

(defn write-random-galaxy []
  (let [galaxy-seed-numbers [(rand-int 65536) (rand-int 65536) (rand-int 65536)]
        galaxy-seeds (take 8 (iterate utility/galaxy-twist (utility/make-seed galaxy-seed-numbers)))
        galaxy-data (map export-galaxy galaxy-seeds (range))
        catalog (map (fn [data seeds index] [(str "Galaxy #" index " : " (seed-as-int seeds) " : " (utility/get-seed-bytes seeds)) data]) galaxy-data galaxy-seeds (range))
        ]
    (utility/write-to-file (.stringify js/JSON (clj->js catalog)) (str "galaxy_random" (seed-as-int (first galaxy-seeds)) ".json"))
    "Wrote Galaxy To Disk"))



(defn write-random-descriptions []
  (let [galaxy-seed-numbers [(rand-int 65536) (rand-int 65536) (rand-int 65536)]
        galaxy-seeds (take 8 (iterate utility/galaxy-twist (utility/make-seed galaxy-seed-numbers)))
        galaxy-names (map export-names galaxy-seeds)
        catalog (map (fn [data seeds index]
                       {:seed (seed-as-int seeds)
                        :data data}
                       ) galaxy-names galaxy-seeds (range))
        
        ]
    (utility/write-to-file (.stringify js/JSON (clj->js catalog)) (str "galaxy_descriptions" (seed-as-int (first galaxy-seeds)) ".json"))
    "Wrote Descriptions To Disk"))


;;(write-random-descriptions)


;; ()
;; ;;(union #{5 6} #{ 5 7})
;; (take 8 (map #(* 256 (* % %)) (range)))
;; (take 8 (map (fn [n] (reduce * (repeat n 256))) (range)))

;; (apply + [7 0 8])
