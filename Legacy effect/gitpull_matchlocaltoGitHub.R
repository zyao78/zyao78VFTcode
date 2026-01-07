# 1) See your current branch (likely 'main')
git rev-parse --abbrev-ref HEAD

# 2) Make sure 'origin' is set (optional)
git remote -v

# 3) Fetch latest from GitHub
git fetch origin

# 4) Switch to the branch you want to mirror (e.g., main)
git switch main  # or: git checkout main

# 5) HARD reset local to remote branch (this overwrites local)
git reset --hard origin/main

# 6) (Optional) remove untracked files/dirs too
git clean -fd