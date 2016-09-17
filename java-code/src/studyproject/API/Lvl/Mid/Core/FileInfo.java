package studyproject.API.Lvl.Mid.Core;

public class FileInfo {

	private String path;
	private long size;
	private String checksum;
	
	public FileInfo(String path, long size, String checksum){
		this.path = path;
		this.size = size;
		this.checksum = checksum;
	}
	
	public String getPath() {
		return path;
	}
	
	public void setPath(String path) {
		this.path = path;
	}
	
	public long getSize() {
		return size;
	}
	
	public void setSize(long size) {
		this.size = size;
	}
	
	public String getChecksum() {
		return checksum;
	}
	
	public void setChecksum(String checksum) {
		this.checksum = checksum;
	}
	
	
}
