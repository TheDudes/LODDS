package studyproject.API.Lvl.Low;

/**
 * class used to store the amount of bytes a client still has to
 * send in
 * @author Michael
 *
 */
public class Load {
	
	private static long currentLoad = 0;
	
	/**
	 * @return
	 * 			the number of bytes that this client still has to send
	 */
	public static synchronized long getCurrentLoad() {
		return currentLoad;
	}

	/**
	 * @param load
	 * 			the number of bytes that this client still has to send
	 */
	@Deprecated
	public static synchronized void setLoad(long load) {
		currentLoad = load;
	}

	/**
	 * @param load
	 * 			the number of bytes that should be added to the current load
	 */
	public static synchronized void incrementLoad(long load) {
		currentLoad += load;
	}

	/**
	 * @param load
	 * 			the number of bytes that should be subtracted from the current load
	 */
	public static synchronized void decrementLoad(long load) {
		if(currentLoad >= load){
			currentLoad =- load;
		}
	}
	
	

}
