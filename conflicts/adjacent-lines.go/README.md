Two branches modify adjacent lines in Go

Git's merge algorithm is confused by branches that immediately adjacent lines.
The patches from the two branches differ in their context lines so Git isn't
sure where to apply the patches.

These kinds of conflicts should be easy to resolve since nobody actually edited
the same lines of code.
