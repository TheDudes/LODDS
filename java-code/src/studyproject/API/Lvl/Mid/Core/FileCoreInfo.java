package studyproject.API.Lvl.Mid.Core;

public class FileCoreInfo {

	private String checksum;
	private long filesize;
	private String fileName;

	public FileCoreInfo(String checksum, long filesize, String fileName) {
		this.checksum = checksum;
		this.filesize = filesize;
		this.fileName = fileName;
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

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	@Override
	public String toString() {
		return getFileName() + ":" + getChecksum() + ":" + getFilesize();
	}

}
