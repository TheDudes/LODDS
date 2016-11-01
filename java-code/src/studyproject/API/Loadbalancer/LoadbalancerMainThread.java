package studyproject.API.Loadbalancer;

import java.util.Collections;
import java.util.Vector;

import studyproject.API.Lvl.Mid.LODDS;
import studyproject.API.Lvl.Mid.Core.UserInfo;

/**
 * 
 * @author Michael
 *
 */
//TODO WIP here, remove later
@SuppressWarnings(value = { "unused" })
public class LoadbalancerMainThread extends Thread {

	private LODDS loddsobject;
	private Vector<ProgressInfo> chunkThreads;
	private String checksum;
	private String localPath;
	private long chunksize;
	private long fileSize;
	private int parallelDownloads;

	public LoadbalancerMainThread(LODDS loddsobject, String checksum,
			String localPath, long fileSize, long chunksize,
			int parallelDownloads) {
		this.loddsobject = loddsobject;
		this.chunksize = chunksize;
		this.checksum = checksum;
		this.localPath = localPath;
		this.parallelDownloads = parallelDownloads;
		this.chunkThreads = new Vector<ProgressInfo>(parallelDownloads);
	}

	@Override
	public void run() {
		LoadComparator loadComparator = new LoadComparator();
		Vector<UserInfo> owningUsers = loddsobject.getOwningUsers(checksum);
		Collections.sort(owningUsers, loadComparator);
		long transmittedData = 0;
		while(transmittedData < fileSize){
			if(chunkThreads.size() < parallelDownloads){
				
			} else{
				
			}
		}
	}

}
