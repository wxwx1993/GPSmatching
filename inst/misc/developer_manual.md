# Contribution

Last Update: 1/27/2021

Contributions are welcome and can come in different forms, including but not limited to:
Adding new features.
Improving documentation.
Addressing one of the open issues.
Creating new issues.
Even fixing typos.
Please read the following documents before making changes to the codebase.

## Environment Setup

In order to contribute to the project, it is a better idea to have your own local copy of  _GPSmatching_ on your Github account. Here are the steps:

- Navigate to GPSmatching Github [repository](https://github.com/fasrc/GPSmatching), and at the top right corner, click on the `Fork` button. This will add a clone of the project to your Github account.
- Open your terminal (or Gitbash for Windows, Anaconda prompt, ...) and run the following command  (brackets are not included):

```S
git clone git@github.com:[your user name]/GPSmatching.git
```
- If you do not already have an SSH key, you need to generate one. Read more [here](https://docs.github.com/en/github-ae@latest/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent).
- Now, you can modify the codebase and track your modification.
- It is a good idea to create a new branch to work on the codebase. Read the following instructions for git branching.

## Git Branching Model

Although, in your personal repository, you can pick any branch name, however, in order to keep consistency and also understand who is working on what, the following convention is strongly recommended.  In this project, we follow the convention that is proposed by Vincent Driessen in his [A successful Git branching model](https://nvie.com/posts/a-successful-git-branching-model/) post.

Here is the summary of the branches:

- **master**: master branch only hosts the released software packages. Only project maintainers have write access on the master branch.
- **develop**: develop branch is considered the main branch where the source code of HEAD always reflects a state with the latest delivered development changes for the next release.
- **supporting** branch: There are different supporting branches, three main supporting branches that we suggest the contributors to follow the naming convention includes:
  - *feature*: we start a new feature branch to add new features to the software. The naming convention is iss[issue_number]_short_description. For example, if I need to add unittest to one of the functions in the package and the issue number is 12, iss12_add_unittest can be a valid git branch name. We start it with the issue number to go back and take a look at the issue details if necessary. Although feature branches are temporary, this naming convention helps developers to understand the situation while working on the codebase. If you are working on some features that there is no open issue for that, please open an issue [here](https://github.com/fasrc/GPSmatching/issues) and assign it to yourself. You can also make a comment that you are working on that. 
  - *hotfix*: hotfix branches will be only used for fixing a bug on a released package. After fixing the bug, the third digit of the version number should be incremented by one. For example, 2.3.5 –> 2.3.6. These branches will be prefixed with hotfix and followed by the upcoming version number (e.g., in this case, hotfix_2.3.6)
  - *release*: Release branches support the preparation of a new production release.

## Where to submit pull requests?

All pull requests should be submitted to `base repository: fasrc/GMSmatching` and `base: develop` branch.

## Pull request checklist

- Please run `devtools::document()`, `devtools::load_all()` after your final modifications.
- Make sure that your modified code passes all checks and tests (you can run `devtools::check()` in RStudio)
- Your PR should pass all the CI and reviews so we can merge it.
- Add a line(s) about the modification to the NEWS.md file.
- If you are adding new features, please make sure that appropriate documentation is added or updated.
- Please clean up white spaces. Read more [here](https://softwareengineering.stackexchange.com/questions/121555/why-is-trailing-whitespace-a-big-deal).

## Style Guide

In this project, we are moving toward [Google's R Style Guide](https://google.github.io/styleguide/Rguide.html).