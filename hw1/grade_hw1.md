*Siyan Chen*

### Overall Grade: 80/100

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? Yes

-   Is the final report in a human readable format html? Yes

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility? Yes

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how are results produced by just reading the report? Yes

### Completeness, correctness and efficiency of solution: 45/50

- Q1 (10/10)

- Q2 (13/20)

For 2.4, the files are gzipped. You need to use `zcat` to get the correct line counts. -5pts. 

Make code skip header to get number of unique patients. -2pts


- Q3 (12/10)

It's fine to just count the lines containing each name. If a student figures out a way to count the words (one line may contain the same name multiple times), give bonus points.

- Q4 (10/10)
	    
### Usage of Git: 5/10

-   Are branches (`master` and `develop`) correctly set up? Is the hw submission put into the `master` branch? Y

-   Are there enough commits? Are commit messages clear?  Y
          
-   Is the hw1 submission tagged? Y

-   Are the folders (`hw1`, `hw2`, ...) created correctly? Y
  
-   Do not put a lot auxiliary files into version control. 

- `pride_and_prejudice.txt` is in Git, along with `test2.txt`. Do not commit these. -5 points.

### Reproducibility: 10/10

-   Are the materials (files and instructions) submitted to the `master` branch sufficient for reproducing all the results? Just click the `knit` button will produce the final `html` on teaching server? Y

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results? Y

### R code style: 10/20

For bash commands, only enforce the 80-character rule. Not followed. 5+ violations.

-   [Rule 3.](https://google.github.io/styleguide/Rguide.xml#linelength) The maximum line length is 80 characters. 

-   [Rule 4.](https://google.github.io/styleguide/Rguide.xml#indentation) When indenting your code, use two spaces.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place spaces around all binary operators (=, +, -, &lt;-, etc.). 
	
-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place a space before a comma, but always place one after a comma. 

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place a space before left parenthesis, except in a function call.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place spaces around code in parentheses or square brackets.
