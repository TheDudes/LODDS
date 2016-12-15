package studyproject.API.Lvl.Mid.ThreadMonitoring;

public interface MonitoredThread {	
	public boolean isSubmitted();

	public void setSubmitted(boolean toSet); 

	/**
	 * 0.00 to 1.00 for finished
	 * 
	 * @param toSet
	 *            the number to set
	 * @return true if successfull
	 */
//	public boolean setProgress(double toSet);

	public long getProgress();

}