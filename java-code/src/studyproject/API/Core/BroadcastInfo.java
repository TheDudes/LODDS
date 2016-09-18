package studyproject.API.Core;

public class BroadcastInfo {

	public String networkAddress;
	public int ipPort;
	public long timestamp;
	public long load;
	public String name;

	public String toString() {
		return networkAddress + " " + String.valueOf(ipPort) + " "
				+ String.valueOf(timestamp) + " " + String.valueOf(load) + " " + name;
	}

}
