# Git Utils

<p>
    <a href="https://github.com/tbidne/git-utils/workflows/stack%20build/badge.svg?branch=master" alt="stack build">
        <img src="https://img.shields.io/github/workflow/status/tbidne/git-utils/stack build/master?logo=haskell&style=plastic"/>
    </a>
    <a href="https://github.com/tbidne/git-utils/workflows/docker%20push/badge.svg?branch=master" alt="docker hub">
        <img src="https://img.shields.io/github/workflow/status/tbidne/git-utils/docker push/master?logo=docker&logoColor=ffffff&style=plastic"/>
    </a>
    <a href="https://hub.docker.com/repository/docker/tbidne/git-utils" alt="docker hub">
        <img src="https://img.shields.io/static/v1?label=docker&message=hub&color=089cec&style=plastic&logo=docker&logoColor=ffffff"/>
    </a>
</p>

Scans a `git` directory for branches based on a grep string then prints all stale branches, organized by author and merge status.

## Stack

To build with stack run `stack build`.

Run with `stack exec git-utils-exe <grep> <directory> <limit>`.

`grep` and `directory` are non-optional, but if no grep string is desired then `""` can be provided.

The default `limit` is 30.

## Docker

A docker image can be downloaded with `docker pull tbidne/git-utils:latest`.

It can also be built manually, e.g.

```docker
Docker build .
```

Run with, say

```docker
docker run -v /path/to/dir:/share --rm <image> grep /share 30
```

**IMPORTANT**: if you are on a mac then you will want to use the `cached` flag, e.g.

```docker
docker run -v /path/to/dir:/share:cached --rm <image> grep /share 30
```

The default docker volume mounting for mac has terrible performance (no arg is equivalent to `consistent` and `default`). `cached` and `delegated` will likely have similar performance, though `cached` theoretically makes more sense given that docker is solely reading data, not writing. In any case, `cached` or `delegated` should dramatically improve performance. Of course if you have stack then you can skip all this and get much better performance than even `cached` ;-).

See [here](https://docs.docker.com/docker-for-mac/osxfs-caching/) for more info.
