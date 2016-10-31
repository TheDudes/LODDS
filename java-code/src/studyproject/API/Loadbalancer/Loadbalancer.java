package studyproject.API.Loadbalancer;

import studyproject.API.Lvl.Mid.LODDS;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class Loadbalancer {

	private LODDS loddsobject;
	private long loadBalancingMinumum;

	public Loadbalancer(LODDS loddsobject, long loadBalancingMinumum) {
		this.loddsobject = loddsobject;
		this.loadBalancingMinumum = loadBalancingMinumum;
	}

	public void splitLoad(String checksum, String localPath) {
		if (loddsobject.getFileSize(checksum) < loadBalancingMinumum) {
			loddsobject
					.getFile(getClientMinLoad(checksum), checksum, localPath);
		} else {
			// TODO start thread that assigns how much is taken from which user,
			// checks the threads to get the parts of the file and, after all
			// threads finished, puts the temporary files together
		}
	}

	/**
	 * 
	 * @return the client that has the specified and has the lowest amount of
	 *         load or the first client in the list that has no load
	 */
	private String getClientMinLoad(String checksum) {
		long minLoad = Long.MAX_VALUE;
		String clientWithLowestLoad = "";
		for (UserInfo userInfo : loddsobject.getAvailableFiles().get(checksum)
				.getOwningUsers()) {
			if (userInfo.getLoad() == 0) {
				return userInfo.getUserName();
			} else if (userInfo.getLoad() < minLoad) {
				clientWithLowestLoad = userInfo.getUserName();
				minLoad = userInfo.getLoad();
			}
		}
		return clientWithLowestLoad;
	}

	/**
	 * 
	 * @return the minimum size before a file is split for load balancing if
	 *         there is more than one client sharing that file
	 */
	public long getLoadBalancingMinumum() {
		return loadBalancingMinumum;
	}

	/**
	 * 
	 * @param loadBalancingMinumum
	 *            the minimum size before a file is split for load balancing if
	 *            there is more than one client sharing that file
	 */
	public void setLoadBalancingMinumum(long loadBalancingMinumum) {
		this.loadBalancingMinumum = loadBalancingMinumum;
	}

}
