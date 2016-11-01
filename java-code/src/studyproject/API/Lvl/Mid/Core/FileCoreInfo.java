package studyproject.API.Lvl.Mid.Core;

public class FileCoreInfo {

	private String checksum;
	private long filesize;

	public FileCoreInfo(String checksum, long filesize) {
		this.checksum = checksum;
		this.filesize = filesize;
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

}
