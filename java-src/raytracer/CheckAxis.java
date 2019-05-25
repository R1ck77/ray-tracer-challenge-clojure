package raytracer;

public class CheckAxis {

	private CheckAxis() {}

	private static double axisIntersection(double numerator, double direction) {
		if (Math.abs(direction) > 1e-6) {
			return numerator / direction;
		} else {
			return numerator > 0? Double.POSITIVE_INFINITY : Double.NEGATIVE_INFINITY;
		}
	}

	private static void checkAxis(double origin, double direction, double[] mins, double[] maxs, int index) {
		double a = axisIntersection(-1-origin, direction);
		double b = axisIntersection(1 - origin, direction);
		if(a > b) {
			mins[index] = b;
			maxs[index] = a;
		} else {
			mins[index] = a;
			maxs[index] = b;
		}
	}

	public static double[] localIntersect(double xorigin, double yorigin, double zorigin,
										  double xdirection, double ydirection, double zdirection) {
		double[] mins = new double[3];
		double[] maxs = new double[3];	   
		checkAxis(xorigin, xdirection, mins, maxs, 0);
		checkAxis(yorigin, ydirection, mins, maxs, 1);
		checkAxis(zorigin, zdirection, mins, maxs, 2);
		double t1 = Math.max(mins[0], Math.max(mins[1], mins[2]));
		double t2 = Math.min(maxs[0], Math.min(maxs[1], maxs[2]));
		if(t1 > t2) {
			return null;
		} else {
			return new double[]{t1, t2};
		}
	}
}
