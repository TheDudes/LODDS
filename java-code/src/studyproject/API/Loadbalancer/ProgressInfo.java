package studyproject.API.Loadbalancer;

import studyproject.API.Lvl.Mid.FileConnectionThread;

/**
 * class to keep track of the different threads used in loadbalancing
 * 
 * @author Michael
 *
 */
public class ProgressInfo {

	private boolean finishedSuccessfully;
	private long startBlock;
	private long endBlock;
	private int chunknumber;
	private String tmpFileName;
	private FileConnectionThread fileConnectionThread;

	/**
	 * Note: finishedSuccessfully is set to false per default, since the thread
	 * should not be done when you create this object to track if it finishes
	 * its job
	 * 
	 * @param startBlock
	 *            the startIndex from which to start
	 * @param endBlock
	 *            the endIndex until which the file gets pulled
	 * @param tmpFileName
	 *            the name of the temporary file
	 * @param fileConnectionThread
	 *            the thread that gets tracked
	 */
	public ProgressInfo(long startBlock, long endBlock, int chunknumber,
			String tmpFileName, FileConnectionThread fileConnectionThread) {
		finishedSuccessfully = false;
		this.startBlock = startBlock;
		this.endBlock = endBlock;
		this.chunknumber = chunknumber;
		this.tmpFileName = tmpFileName;
		this.fileConnectionThread = fileConnectionThread;
	}

	/**
	 * 
	 * @return reference to the thread being tracked in this object
	 */
	public FileConnectionThread getFileConnectionThread() {
		return fileConnectionThread;
	}

	/**
	 * 
	 * @return did the thread finish its download successfully
	 */
	public boolean isFinishedSuccessfully() {
		return finishedSuccessfully;
	}

	/**
	 * 
	 * @param finishedSuccessfully
	 *            did the thread finish its download successfully
	 */
	public void setFinishedSuccessfully(boolean finishedSuccessfully) {
		this.finishedSuccessfully = finishedSuccessfully;
	}

	/**
	 * 
	 * @return the index from which the thread starts downloading the file
	 */
	public long getStartBlock() {
		return startBlock;
	}

	/**
	 * 
	 * @param startBlock
	 *            the index from which the thread starts downloading the file
	 */
	public void setStartBlock(long startBlock) {
		this.startBlock = startBlock;
	}

	/**
	 * 
	 * @return the last index the thread downloads of the file
	 */
	public long getEndBlock() {
		return endBlock;
	}

	/**
	 * 
	 * @param endBlock
	 *            the last index the thread downloads of the file
	 */
	public void setEndBlock(long endBlock) {
		this.endBlock = endBlock;
	}

	/**
	 * 
	 * @return the name
	 */
	public String getTmpFileName() {
		return tmpFileName;
	}

	public void setTmpFileName(String tmpFileName) {
		this.tmpFileName = tmpFileName;
	}

	/**
	 * 
	 * @return the number describing the position of this particular chunk,
	 *         numbering starts at 0
	 */
	public int getChunknumber() {
		return chunknumber;
	}

}
