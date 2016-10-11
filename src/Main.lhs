Generator of URLs for downloading Debian repositories's indexes 
======
The following program comes out from the necessity
of installing Debian packages in an offline machine.
The procedure for installing them where this program 
becomes useful is the following:

- Download the repository index in an online machine 
and take it to the offline machine in an USB drive.
- Add a repository to apt's configuration whose
location is that of the local copy of the index. The
index can be considered a fake repository, useful only 
for updating apt's cache.
- Update apt's cache (apt update).
- With Synaptic package manager, mark packages for
being installed, then instead of applying the changes
use Synaptic's option of generating a download script.
- Since the download script contains URLs pointing
to the local fake repository, we change the part of those
URLs referring to the local repository to refer the online
repository.
- We take the script to the online machine, with wget
and the script properly set for the script's execution 
(in Windows I change the script extension to "bat",
the Unix line ending to DOS line ending, put 
wget.exe in the same folder the script is, and change 
every call to "wget" in the script to "wget.exe"); and
we execute it.
- We take the downloaded packages to the offline machine,
create with them a local repository, add the repository 
to sources.list, update the cache and install the
packages.

This program automates the first step of the previous 
algorithm. Since we know the structure of a Debian
repository, is easy to generate the online URLs of the index 
components given some parameters. Since wget can create 
the folder structure, where the files are in the server, 
locally (with option -nH); there's no need of creating 
the directory structure before. Actually wget can do 
recursive downloads, which would make this program useless,
but the advantage of this program is the "accuracy" of generated
URLs; in a recursive download with wget, avoiding links not 
conforming the repository structure is tricky, since there is
no standard HTML page for presenting a repository over HTTP.

> module Main where

Appendix A: Imports
----
Regarding this programming style I got a complaint
over the literate preprocessor: I should be able 
to put the imports near the module segment using them or at
the end of the module, since they are determined by the 
program text. The exports's place is the beginning 
since they are the module's interface, i.e. the program 
text is determined by them.

> import Data.Bifunctor (bimap)
> import System.Environment (getArgs)
> import System.IO (hPutStrLn, stderr, withFile, IOMode( WriteMode ))
> import Data.List (isPrefixOf, foldl')
> import Data.List.Split (splitOn)
> import Control.Applicative (liftA2, liftA3)
> import Data.Char (isLower, isAlphaNum)
> import Text.Printf (printf)
> import System.FilePath.Posix ((</>))
> import Data.Bool.Predicates (lBool, lAnd, lOr)

Command line interface 
---
The program receives a list of arguments described in the next 
section. It writes the URLs of files to be downloaded to standard 
output, or an error message to standard error.

> main :: IO ()
> main = getArgs >>= either (hPutStrLn stderr) genURLs . parseArgs

> help :: String
> help = "\n\
> \Arguments order and description:\n\
> \ \n\
> \ \"online-repository\": URL of the repository, like \"http://debian.org\"\n\
> \ \"online-repository-root\": Directory containing the repository, i.e.\n\
> \ the directory having dists,pool as subdirectories.\n\
> \ \"distribution-version-name\": A word. Examples \"xenial\",\"jessie\", etc.\n\
> \ \"branches\": A sequence of comma separated words, \n\
> \ branches ::= word {\",\" word}. Example: \"main,contrib,non-free\".\n\
> \ \"distribution-architectures\": A sequence of comma separated words.\n\
> \ archs ::= word {\",\" word}. Example \"amd64,i386\".\n\
> \ \n\
> \Operation:\n\
> \ Writes to standard output the URLs of the files\n\
> \ to be downloaded by a tool like wget. Or writes an error message,\n\
> \ or this help message to standard error.\n"

 ### Arguments order and description 

"online-repository": URL of the repository, like "http://debian.org"
"online-repository-root": Directory containing the repository, i.e.
the directory having dists,pool as subdirectories.
"distribution-version-name": A word. Examples "xenial","jessie", etc.
"branches": A sequence of comma separated words, 
branches ::= word {"," word}. Example: "main,contrib,non-free".
"distribution-architectures": A sequence of comma separated words.
archs ::= word {"," word}. Example "amd64,i386".

> parseArgs :: [String] -> Either String Args
> parseArgs (a:b:c:d:e:[]) = Args <$> checkURL a <*> checkRoot b 
>   <*> checkDist c <*> checkBranches d <*> checkArchs e
> parseArgs _ = Left ("Wrong number of arguments." ++ help)

_Remark_ In the previous function's first pattern match appears
an expression that seems a fold, with default value Args, some 
lifting of <*> as binary operator and a list of functions to be
folded. But so far my attempts to make this have failed.
_End of remark_

> data Args = Args URL Root Dist Branches Archs deriving (Show)
> type URL = String
> type Root = String
> type Dist = String
> type Branches = [String]
> type Archs = [String]

The following tests are not as exact as they could be, but
so far that is not a big problem, since almost all the users 
(by my informal statistic analysis of users knowledge)
will recognize what is wrong when the download script fails. I
think they are effective catching some common typos.

> checkURL :: String -> Either String URL
> checkURL = check (isPrefixOf "http://") "Error parsing URL"

> checkRoot :: String -> Either String Root
> checkRoot = check isName "Error in root directory name"

> checkDist :: String -> Either String Dist
> checkDist = check isName "Error in distribution name"

> checkBranches :: String -> Either String Branches
> checkBranches = check (all isName) "Error in branch name" . 
>   splitOn ","

> checkArchs :: String -> Either String Archs
> checkArchs = check (all isName) "Error in architecture name" . 
>   splitOn ","

> isName :: String -> Bool
> isName = ((/=0) . length) `lAnd` all (isLower `lOr` isAlphaNum `lOr` 
>   (== '-'))

The check function returns a formatted error message containing 
the checked String at the end, wrapped in Left, when the predicate 
returns False; otherwise returns the checked String wrapped in Right.

> check :: Show a => (a -> Bool) -> String -> a -> Either String a
> check p msg = bimap (printf "%s %s " msg . show) id . 
>   lBool Left Right p

The core of the program 
---
Template defines the repository structure. It is noted
that a repository is not as general as a directory tree.
Each directory in determined level has always the same 
file and directory names inside. That means the list of
children can be considered a single element if we
define a value of the tree being parent of another like 
every element of that compound value be parent of every
element of the compound children value. This way 
the tree looks flat and we can represent directory hierarchy 
using a list by putting the children after its parents in
the list order. The Comp type separates directories 
from files conveniently since files aren't parents,
and are treated differently. The files in a Comp
structure are the children files of every directory
in the structure, not the children of the parent 
directories. This is explained next.

In an early design of the program, the function for 
filling the template with directories was more general.
It was meant to be used for filling an a template with
an arbitrary number of directories, where each empty list 
was the place for substituting with the correspondent 
directory list. In that context the last component could 
not have an empty list in the place of children directories,
since that would represent an ambiguity. There's no interference 
of the early design with the current design, therefore is not
changed.

> type Comp = (Dirs, Files) -- Comp is an abbreviation of Component
> type Dirs = [FilePath] -- The name of the folders
> type Files = [FilePath] -- The files inside those folder

All arguments must be substituted in the template according 
particular rules, except the first, which is the repository URL.

> template :: Root -> Dist -> Branches -> Archs -> [Comp]
> template r d b a = [([r], ["Release.gpg"]), -- root
>   (["dists"], []),
>   ([d], ["InRelease", "Release"]),          -- distribution
>   (b, ["Release"]),                         -- branches
>   (map ("binary-"++) a, ["Packages.xz", "Packages.gz", -- architectures
>       "Packages.bz2", "Packages"])]

 ###genURLs

Fills the template with the parsed arguments and writes the URLs 
generated by paths to standard output.

> genURLs :: Args -> IO ()
> genURLs (Args u r d b a) = wrtURLs (paths ("", u) (template r d b a))

> wrtURLs :: Files -> IO ()
> wrtURLs fs = mapM_ putStrLn fs

 ###paths

Gets all file URL's. The current Comp (head of the list given as second 
parameter) has p as parent, but the files in that element (y) are 
the children of each directory in the element (each element in x).
Each element in ds is the next parent in the recursive call. All file paths 
generated by the recursive call are accumulated, starting with the 
file paths of the current Comp (fs).

> paths :: (FilePath, URL) -> [Comp] -> Files
> paths (p, u) [(x,y)] = childrenURLs u y $ joinP p x
> paths (p, u) ((x,y):xs) = foldl' (\a b -> a ++ paths (b, u) xs) fs ds
>   where
>   ds = joinP p x
>   fs = childrenURLs u y ds
> paths (_,_) [] = []

> childrenURLs :: URL -> Files -> Dirs -> Files
> childrenURLs u fns ds = concat $ map (\i -> childrenURL (i, u) fns) ds

The following constructs a list of file paths with root u/p;
because files are meant to be downloaded from URL u.

> childrenURL :: (FilePath, URL) -> Files -> Files
> childrenURL (p, u) = (head (u `joinP` [p]) `joinP`)

> joinP :: FilePath -> [FilePath] -> [FilePath]
> joinP p c = map (p </>) c