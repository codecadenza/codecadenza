/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.buildtest.mapping;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import java.io.Serializable;
import net.codecadenza.eclipse.model.project.BuildToolEnumeration;
import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.ServerPlatformEnumeration;
import net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration;

/**
 * <p>
 * JAXB mapping class for project configuration elements
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class ProjectConfiguration implements Serializable {
	private static final long serialVersionUID = -931153944730921314L;

	@XmlAttribute(name = "clp", required = true)
	private ClientPlatformEnumeration clientPlatform;

	@XmlAttribute(name = "spl", required = true)
	private ServerPlatformEnumeration serverPlatform;

	@XmlAttribute(name = "pp", required = true)
	private PersistenceProviderEnumeration persistenceProvider;

	@XmlAttribute(name = "tpl", required = true)
	private TechnologyPlatformEnumeration technologyPlatform;

	@XmlAttribute(name = "bm", required = true)
	private boolean boundaryMode;

	@XmlAttribute(name = "bt", required = true)
	private BuildToolEnumeration buildTool;

	@XmlAttribute(name = "template", required = true)
	private String projectTemplateName;

	private boolean protectManualChanges;
	private boolean addREST;
	private boolean addSOAP;
	private boolean addRMI;
	private boolean addKafka;
	private boolean addJMS;
	private boolean addSelenium;

	/**
	 * @return the client platform
	 */
	public ClientPlatformEnumeration getClientPlatform() {
		return this.clientPlatform;
	}

	/**
	 * @param clientPlatform
	 */
	public void setClientPlatform(ClientPlatformEnumeration clientPlatform) {
		this.clientPlatform = clientPlatform;
	}

	/**
	 * @return the server platform
	 */
	public ServerPlatformEnumeration getServerPlatform() {
		return this.serverPlatform;
	}

	/**
	 * @param serverPlatform
	 */
	public void setServerPlatform(ServerPlatformEnumeration serverPlatform) {
		this.serverPlatform = serverPlatform;
	}

	/**
	 * @return the persistence provider
	 */
	public PersistenceProviderEnumeration getPersistenceProvider() {
		return this.persistenceProvider;
	}

	/**
	 * @param persistenceProvider
	 */
	public void setPersistenceProvider(PersistenceProviderEnumeration persistenceProvider) {
		this.persistenceProvider = persistenceProvider;
	}

	/**
	 * @return the technology platform
	 */
	public TechnologyPlatformEnumeration getTechnologyPlatform() {
		return this.technologyPlatform;
	}

	/**
	 * @param technologyPlatform
	 */
	public void setTechnologyPlatform(TechnologyPlatformEnumeration technologyPlatform) {
		this.technologyPlatform = technologyPlatform;
	}

	/**
	 * @return true if the boundary mode is set
	 */
	public boolean isBoundaryMode() {
		return this.boundaryMode;
	}

	/**
	 * @param boundaryMode
	 */
	public void setBoundaryMode(boolean boundaryMode) {
		this.boundaryMode = boundaryMode;
	}

	/**
	 * @return the project template name
	 */
	public String getProjectTemplateName() {
		return this.projectTemplateName;
	}

	/**
	 * @param projectTemplateName
	 */
	public void setProjectTemplateName(String projectTemplateName) {
		this.projectTemplateName = projectTemplateName;
	}

	/**
	 * @return the project name
	 */
	public String getProjectName() {
		return buildProjectName();
	}

	/**
	 * @return the build tool
	 */
	public BuildToolEnumeration getBuildTool() {
		return buildTool;
	}

	/**
	 * @param buildTool
	 */
	public void setBuildTool(BuildToolEnumeration buildTool) {
		this.buildTool = buildTool;
	}

	/**
	 * @return true if manual changes in the source code should be protected
	 */
	public boolean isProtectManualChanges() {
		return protectManualChanges;
	}

	/**
	 * @param protectManualChanges
	 */
	public void setProtectManualChanges(boolean protectManualChanges) {
		this.protectManualChanges = protectManualChanges;
	}

	/**
	 * @return true if a REST integration module should be added
	 */
	public boolean isAddREST() {
		return addREST;
	}

	/**
	 * @param addREST
	 */
	public void setAddREST(boolean addREST) {
		this.addREST = addREST;
	}

	/**
	 * @return true if a SOAP integration module should be added
	 */
	public boolean isAddSOAP() {
		return addSOAP;
	}

	/**
	 * @param addSOAP
	 */
	public void setAddSOAP(boolean addSOAP) {
		this.addSOAP = addSOAP;
	}

	/**
	 * @return true if a RMI integration module should be added
	 */
	public boolean isAddRMI() {
		return addRMI;
	}

	/**
	 * @param addRMI
	 */
	public void setAddRMI(boolean addRMI) {
		this.addRMI = addRMI;
	}

	/**
	 * @return true if a Kafka integration module should be added
	 */
	public boolean isAddKafka() {
		return addKafka;
	}

	/**
	 * @param addKafka
	 */
	public void setAddKafka(boolean addKafka) {
		this.addKafka = addKafka;
	}

	/**
	 * @return true if a JMS integration module should be added
	 */
	public boolean isAddJMS() {
		return addJMS;
	}

	/**
	 * @param addJMS
	 */
	public void setAddJMS(boolean addJMS) {
		this.addJMS = addJMS;
	}

	/**
	 * @return true if a Selenium test module should be added
	 */
	public boolean isAddSelenium() {
		return addSelenium;
	}

	/**
	 * @param addSelenium
	 */
	public void setAddSelenium(boolean addSelenium) {
		this.addSelenium = addSelenium;
	}

	/**
	 * @return the project name
	 */
	public String buildProjectName() {
		var name = "";

		if (clientPlatform == null)
			return name;

		if (clientPlatform != ClientPlatformEnumeration.NONE) {
			if (clientPlatform == ClientPlatformEnumeration.JSF_PRIMEFACES)
				name = "JSF";
			else
				name = clientPlatform.name();

			name += "-";
		}

		name += projectTemplateName;
		name += "-";

		if (technologyPlatform == TechnologyPlatformEnumeration.JAKARTA_EE)
			name += "EE";
		else if (technologyPlatform == TechnologyPlatformEnumeration.JAVA_SE)
			name += "SE";
		else
			name += "SPB";

		name += "-";

		if (!boundaryMode)
			name += "FA-";

		if (serverPlatform == ServerPlatformEnumeration.GLASSFISH)
			name += "GL-";
		else if (serverPlatform == ServerPlatformEnumeration.JBOSS)
			name += "JB-";
		else if (serverPlatform == ServerPlatformEnumeration.TOMCAT)
			name += "TO-";

		if (persistenceProvider == PersistenceProviderEnumeration.ECLIPSELINK)
			name += "ECL";
		else
			name += "HIB";

		return name;
	}

}
