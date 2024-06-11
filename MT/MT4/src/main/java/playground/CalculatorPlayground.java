package playground;

import generated.calculator.CalculatorLexer;
import generated.calculator.CalculatorParser;

import java.text.ParseException;

public class CalculatorPlayground {

    public static void main(String[] args) throws ParseException {
        String line = "0 + (-1.0) - 1";
        CalculatorLexer lexer = new CalculatorLexer(line);
        System.out.println(new CalculatorParser(lexer).eTerm().value);
    }
}
