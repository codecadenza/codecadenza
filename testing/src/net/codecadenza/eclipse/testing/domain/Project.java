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
package net.codecadenza.eclipse.testing.domain;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * Domain object for projects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Project {
	public static final String ADDITIONAL_NAMESPACE = "add";
	public static final String DEFAULT_NAMESPACE = "base";
	private static final String DIAGRAM_FILE_SUFFIX = ".ccd";

	private final String name;
	private final TechnologyPlatform technologyPlatform;
	private final ClientPlatform clientPlatform;
	private final List<DomainObject> domainObjects = new ArrayList<>();
	private final List<Enumeration> enumerations = new ArrayList<>();
	private final List<DataExchangeMethod> dataExchangeMethods = new ArrayList<>();
	private DataSource dataSource;

	/**
	 * Constructor
	 * @param name
	 * @param technologyPlatform
	 * @param clientPlatform
	 */
	public Project(String name, TechnologyPlatform technologyPlatform, ClientPlatform clientPlatform) {
		this.name = name;
		this.technologyPlatform = technologyPlatform;
		this.clientPlatform = clientPlatform;
	}

	/**
	 * @return the name of the project
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the technology platform
	 */
	public TechnologyPlatform getTechnologyPlatform() {
		return technologyPlatform;
	}

	/**
	 * @return the client platform
	 */
	public ClientPlatform getClientPlatform() {
		return clientPlatform;
	}

	/**
	 * @return a list that contains all domain objects
	 */
	public List<DomainObject> getDomainObjects() {
		return domainObjects;
	}

	/**
	 * @return the data source
	 */
	public DataSource getDataSource() {
		return dataSource;
	}

	/**
	 * @param dataSource
	 */
	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}

	/**
	 * Get the domain objects of the given namespace
	 * @param namespace
	 * @return a list that contains the domain objects of the given namespace
	 */
	public List<DomainObject> getDomainObjectsOfNamespace(String namespace) {
		return domainObjects.stream().filter(d -> d.getNamespace().equals(namespace)).toList();
	}

	/**
	 * @return all non-abstract domain objects of this project
	 */
	public List<DomainObject> getNonAbstractDomainObjects() {
		return new ArrayList<>(domainObjects.stream().filter(d -> !d.isMappedSuperClass()).toList());
	}

	/**
	 * @return a list that contains all enumerations
	 */
	public List<Enumeration> getEnumerations() {
		return enumerations;
	}

	/**
	 * Get the enumerations of the given namespace
	 * @param namespace
	 * @return a list that contains the enumerations of the given namespace
	 */
	public List<Enumeration> getEnumerationsOfNamespace(String namespace) {
		return enumerations.stream().filter(d -> d.getNamespace().equals(namespace)).toList();
	}

	/**
	 * @return all data exchange methods
	 */
	public List<DataExchangeMethod> getDataExchangeMethods() {
		return dataExchangeMethods;
	}

	/**
	 * @return true if GUI tests should be added
	 */
	public boolean addGUITests() {
		return clientPlatform == ClientPlatform.JSF_PRIMEFACES || clientPlatform == ClientPlatform.VAADIN;
	}

	/**
	 * @return true if an additional namespace is required
	 */
	public boolean hasAdditionalNamespace() {
		return domainObjects.stream().map(DomainObject::getNamespace).anyMatch(n -> n.equals(ADDITIONAL_NAMESPACE));
	}

	/**
	 * Get the name of the domain diagram file for the given namespace
	 * @param namespace
	 * @return the name of the domain diagram file
	 */
	public String getDomainDiagramFileName(String namespace) {
		return "package-" + namespace + DIAGRAM_FILE_SUFFIX;
	}

}
