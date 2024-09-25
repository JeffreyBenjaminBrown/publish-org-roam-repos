# Purpose

To take a collection of interlinked org-roam repositories
(likely with different permissions or intended audiences),
and populate a corresponding set of repositories with .org files,
such that the org-roam links have been rewritten as URLs.
This allows you (or anyone else with access)
to navigate the links between files
by simply clicking on the link as Github shows it.

# Usage

You'll need to modify `app/Config.hs` to reflect your data.
In the example data (mine), there are four repos;
your data will probably be different.

That means redefining these four objects:

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
