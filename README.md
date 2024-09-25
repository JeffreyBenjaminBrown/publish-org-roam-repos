# Purpose

To take a collection of interlinked org-roam repositories
(likely with different permissions or intended audiences),
and populate a corresponding set of repositories with .org files,
such that the org-roam links have been rewritten as URLs.
This allows you (or anyone else with access)
to navigate the links between files
by simply clicking on the link as Github shows it.

# Usage

## Install Haskell and Cabal

If they aren't installed, install Haskell and Cabal.
(They're usually a package deal --
Cabal is the most popular build system for Haskell.)

## Configure the code to understand your data

Modify `app/Config.hs` to reflect your data.
Your data consists of three parts:
Offline source org-roam repositories (which won't be changed),
offline destination ("org-git", we could call them) repositories,
and the online destination repositories.

In the example `Config.hs` provided, there are four `Repo` objects.
Each `Repo` consists of three paths,
corresponding to the three parts described above.

In `Config.hs` the four definitions you might want to change are:
```
offlinePrefix_source      :: FilePath
offlinePrefix_destination :: FilePath
onlinePrefix              :: FilePath
repos                     :: M.Map String Repo
```

The destination repos at `Config.offlinePrefix_destination`
must already exist.
If any source org-roam repo includes subfolders,
the destination repo must include subfolders to match.
(This could be handled in the code but so far it isn't.)

(Actually, you only need to define `repos`.
The other three definitions are just to make that one more readable,
and depending on how you organize your data,
it might be impossible to define them.
For instance, if no single folder contains all your source repos,
then a single `offlinePrefix_source` cannot be defined.)

## Run it

Run `cabal run` from the root of the repository.
The first time it will compile and run.
On subsequent runs it won't need to compile first,
unless you change something (but the compilation will be fast).
Exporting my repos (which are about 5.3 million characters)
takes less than 10 seconds on a not-particularly-beefy laptop.

## Commit and push the destination repos

This doesn't do any of that --
all it does is rewrite the org-roam files in a new format.
