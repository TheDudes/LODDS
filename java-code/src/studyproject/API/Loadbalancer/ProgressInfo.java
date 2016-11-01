package studyproject.API.Loadbalancer;

public class ProgressInfo {

	private boolean finishedSuccessfully;
	private long startBlock;
	private long endBlock;
	private String tmpFileName;

	public boolean isFinishedSuccessfully() {
		return finishedSuccessfully;
	}

	public void setFinishedSuccessfully(boolean finishedSuccessfully) {
		this.finishedSuccessfully = finishedSuccessfully;
	}

	public long getStartBlock() {
		return startBlock;
	}

	public void setStartBlock(long startBlock) {
		this.startBlock = startBlock;
	}

	public long getEndBlock() {
		return endBlock;
	}

	public void setEndBlock(long endBlock) {
		this.endBlock = endBlock;
	}

	public String getTmpFileName() {
		return tmpFileName;
	}

	public void setTmpFileName(String tmpFileName) {
		this.tmpFileName = tmpFileName;
	}

}
