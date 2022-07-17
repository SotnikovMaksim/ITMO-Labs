public class Sum {
	public static void main(String[] args) {
		int result = 0; 
		char minus = '-', plus = '+';
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
				} 
				if ((i == arg.length() - 1 && number.length() > 0) ||
				 (Character.isDigit(arg.charAt(i)) == false && number.length() > 0 && arg.charAt(i) != minus)) {
					int n = Integer.parseInt(number.toString());
					if (sign == true) {
						result += n;
					} else {
						result -= n;
					}
					number = new StringBuilder();
					sign = true;
				}
			}
		} 
		System.out.println(result);
	}
}
