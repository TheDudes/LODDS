package studyproject.API.Lvl.Mid.Core;

public class FileCoreInfo {

	private String checksum;
	private long filesize;
	private String filePath;
	private String fileName;

	public FileCoreInfo(String checksum, long filesize, String filePath) {
		this.checksum = checksum;
		this.filesize = filesize;
		this.filePath = filePath;
		this.fileName = filePath.split("/")[(filePath.split("/")).length - 1];
	}

	public FileCoreInfo(String fileName) {
		this.filePath = fileName;
	}

	public String getChecksum() {
		return checksum;
	}

	public void setChecksum(String checksum) {
		this.checksum = checksum;
	}

	public long getFilesize() {
		return filesize;
	}

	public void setFilesize(long filesize) {
		this.filesize = filesize;
	}

	public String getFileName() {
		return fileName;
	}

	public String getFilePath() {
		return filePath;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public void setFilePath(String filePath) {
		this.filePath = filePath;
	}

	@Override
	public String toString() {
		return fileName;
	}

}
