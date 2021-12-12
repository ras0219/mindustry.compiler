#pragma once

#define UNWRAP(X)                                                                                                      \
    do                                                                                                                 \
    {                                                                                                                  \
        if ((rc = (X))) goto fail;                                                                                     \
    } while (0)
