grammar TextNumber;

/*
 * Parser Rules
 */

compileUnit : expression+ EOF;

expression :
	unit hundrer and expression #bigexpression
	| doubleunit hundrer and expression #bigexpression
	| tenner and unit
	| doubleunit
	| unit
	; 

unit : 
	DIGIT # DigitUnit
	| UNIT # WrittenUnit
	| /* epsilon */
	;

doubleunit:
	DOUBLE_UNIT #DoubleUnit
	| DOUBLE_UNIT_DIGITS
	;

tenner:
	TWO_DIGITS # DigitTenner
	| TENNER UNIT
	;

hundrer:
	UNIT HUNDRER 
	| DOUBLEUNIT HUNDRER
	| TENNER HUNDRER
	| HUNDRER
	;
	
and:
	"and"
	| /* epsilon */
	;

/*
 * Lexer Rules
 */

SCALE_NUMBER : ONE_OR_TWO_DIGITS; 
DIGIT: '0'+('1'..'9')

DOUBLE_UNIT_DIGITS: '0'+('1')('1'..'9')
TWO_DIGITS: '0'+('1'..'9')('0'..'9');  

UNIT: 'zero' | 'one' | 'two' | 'three' | 'four' | 'five' | 'six' | 'seven' | 'eight' | 'nine';
DOUBLE_UNIT : 'ten' | 'eleven' | 'twelve' | 'thirteen' | 'fourteen' | 'fifteen' | 'sixteen' | 'seventeen' | 'eightteen' | 'nineteen' ;
TENNER: 'ten' | 'twenty' | 'thirty' | 'forty' | 'fifty' | 'sixty' | 'seventy' | 'eighty' | 'ninety' ;
HUNDRER: 'hundred' | 'thousand' | 'million' ;

WS : [ \t\r\n] -> channel(HIDDEN);