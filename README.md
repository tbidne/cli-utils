# Git Utils

<p>
    <a href="https://github.com/tbidne/git-utils/workflows/stack%20build/badge.svg?branch=master" alt="stack build">
        <img src="https://img.shields.io/github/workflow/status/tbidne/git-utils/stack build/master?logo=haskell&style=plastic" height="20"/>
    </a>
    <a href="https://github.com/tbidne/git-utils/workflows/docker%20push/badge.svg?branch=master" alt="docker hub">
        <img src="https://img.shields.io/github/workflow/status/tbidne/git-utils/docker push/master?logo=docker&logoColor=white&style=plastic" height="20"/>
    </a>
    <a href="https://hub.docker.com/repository/docker/tbidne/git-utils" alt="docker hub">
        <img src="https://img.shields.io/static/v1?label=docker&message=hub&color=089cec&style=plastic&logo=docker&logoColor=white" height="20"/>
    </a>
</p>

Scans a `git` directory for branches based on a grep string and then prints all stale branches, organized by author and merge status.

## Stack

To build with stack run `stack build`.

Run with `stack exec git-utils-exe [--grep=<string>] [--path=<path/to/dir>] [--limit=<days> [--branchType=<a[ll]|r[emote]|l[ocal]>`.

All arguments are optional.

`--grep` is for filtering branches by name, defaults to the empty string, equivalent to passing `--grep=`.

`--path` is the path to the git directory. Defaults to `/share` (mainly used for docker convenience).

 `--limit` is the number of days used for determining if a branch is stale, i.e.,

 ```
 branch is stale iff age(branch) > limit
 ```

The default is 30, and any provided argument must be a non-negative integer.

`--branchType` is the type of branches to search, must be one of `a`, `all`, `r`, `remote`, `l`, and `local`. Defaults to remote.

## Docker

A docker image can be downloaded with `docker pull tbidne/git-utils:latest`.

It can also be built manually, e.g.

```docker
Docker build .
```

To run you must mount your local filesystem with `-v`. The most convenient method is to use the default path of `/share`, i.e.

```docker
docker run -v /path/to/dir:/share --rm <image> <args...>
```

**IMPORTANT**: if you are on a mac then you will want to use the `cached` flag, e.g.

```docker
docker run -v /path/to/dir:/share:cached --rm <image> --grep= --limit=30
```

The default docker volume mounting for mac has terrible performance, so docker has mitigated this by providing volume options. The options most likely to help are `cached` and `delegated`. Theoretically `cached` makes more sense given that docker is solely reading data, not writing it, but local benchmarks show minimal difference between the two. No argument is equivalent to `consistent` (usually aliased by `default`), which you definitely do not want.

See [here](https://docs.docker.com/docker-for-mac/osxfs-caching/) for more info.
