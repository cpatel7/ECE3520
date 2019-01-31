// Library Declaration Section
#include "lambda1.tab.c"
#include "lex.yy.c"
#include "yyerror1.c"


int main()
{
	yyparse();
    
	return(1);
}
