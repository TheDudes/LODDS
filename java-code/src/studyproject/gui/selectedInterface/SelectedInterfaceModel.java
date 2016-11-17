package studyproject.gui.selectedInterface;

import javax.annotation.PostConstruct;

public class SelectedInterfaceModel {
	String selectedInterfaceName;
	StringBuilder networkAddress;
	StringBuilder broadcastAddress;
	int bufferSize;
	int broadcastPort;
	int outgoingPort;

	@PostConstruct
	public void init() {
		networkAddress = new StringBuilder();
		broadcastAddress = new StringBuilder();
		selectedInterfaceName = "";
		bufferSize = 0;
		broadcastPort = 0;
		outgoingPort = 0;
	}
	
	public StringBuilder getBroadcastAddress() {
		return broadcastAddress;
	}

	public void setBroadcastAddress(StringBuilder broadcastAddress) {
		this.broadcastAddress = broadcastAddress;
	}

	public int getBufferSize() {
		return bufferSize;
	}
	
	public void setBufferSize(int bufferSize) {
		this.bufferSize = bufferSize;
	}
	
	public int getBroadcastPort() {
		return broadcastPort;
	}

	public void setBroadcastPort(int broadcastPort) {
		this.broadcastPort = broadcastPort;
	}

	public int getOutgoingPort() {
		return outgoingPort;
	}

	public void setOutgoingPort(int outgoingPort) {
		this.outgoingPort = outgoingPort;
	}

	public String getSelectedInterfaceName() {
		return selectedInterfaceName;
	}

	public void setSelectedInterfaceName(String selectedInterfaceName) {
		this.selectedInterfaceName = selectedInterfaceName;
	}

	public StringBuilder getNetworkAddress() {
		return networkAddress;
	}

	public void setNetworkAddress(StringBuilder networkAddress) {
		this.networkAddress = networkAddress;
	}

}