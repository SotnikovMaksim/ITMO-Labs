public class SumLong {
	public static void main(String[] args) {
		long result = 0; 
		char minus = "-".charAt(0), plus = "+".charAt(0);
		for (String arg : args) { 
			StringBuilder number = new StringBuilder();
			boolean sign = true;
			for (int i = 0; i < arg.length(); i++) { 
				if (Character.isDigit(arg.charAt(i)) == true) {
					number.append(arg.charAt(i)); 
				} else if (arg.charAt(i) == minus) {
					sign = false;
				} else if (arg.charAt(i) == plus) {
					sign = true;
				} if ((i == arg.length() - 1 && number.length() > 0) ||
				 (Character.isDigit(arg.charAt(i)) == false && number.length() > 0 && arg.charAt(i) != minus)) {
					long n = Long.parseLong(number.toString());
					if (sign == true) {
						result += n;
					} else {
						result -= n;
					}
					number = new StringBuilder();
					sign = true;
				}
			}
		} System.out.println(result);
	}
}
