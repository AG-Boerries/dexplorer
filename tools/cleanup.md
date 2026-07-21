# Clean up of this project

## Main idea
I want to clean up this project/R package together with you in a highly interactive way. This should eventually improve clarity, usability, functionality, documentation and reduce dependencies.

## General
Ask if you need clarification. Don't assume anything, make sure you derive information only from code, context or interaction with me.

## Tasks

### Understand
You should read the entire code base and understand it and how the functions relate to each other.

1. I want you to derive a consensus for function documentation and code comments, which we will apply together, when we go over all the files again. So you might want to turn this into a skill to be used later.
2. Flag sections that are not DRY, so that we can address them later.
3. Flag sections, where we can change/optimize code to minimize package dependencies.

### Improve

I want to go with you through each an every file again, you will address the following points, make a suggestion on how you would implement them, ask me for permission to add the changes or if I want something different.

1. Update function documentation and use `match.arg` and `stopifnot` to validate that function parameters are correct. 
2. Update/add code comments.
3. Extract css into the style sheets where possible and reasonable and use bootswatch utility classes where possible and reasonable.
4. Make code also across files DRY.
5. Try to reduce package dependecies.

