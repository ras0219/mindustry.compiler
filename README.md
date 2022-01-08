# mindustry.compiler

A work-in-progress C compiler targeting x86_64.


After being self hosting, the objective is to provide...





## stack layout

| incoming argument overflow ^^^
+----- begin of frame
| return address (8)
| argument storage (8 x 6)
| locals (?)
| temps (?)
| called argument overflow (max N - 6)
+----- %rsp