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
package net.codecadenza.eclipse.resource.dependency;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * JAXB mapping class for dependency activation filter objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlType(name = "ActivationFilterType")
@XmlAccessorType(XmlAccessType.FIELD)
public class FilterMappingType implements Serializable {
	private static final long serialVersionUID = 1L;

	@XmlAttribute(required = true)
	private String clientPlatform;

	@XmlAttribute(required = true)
	private String technology;

	@XmlAttribute(required = true)
	private String serverPlatform;

	@XmlAttribute(required = true)
	private String validationType;

	@XmlAttribute(required = true)
	private String database;

	@XmlAttribute(required = true)
	private String jpaProvider;

	@XmlAttribute(required = false)
	private String description;

	@XmlElement(name = "dependency", required = false)
	private List<DependencyMappingType> dependencies = new ArrayList<>();

	/**
	 * @return the client platform filter string
	 */
	public String getClientPlatform() {
		return this.clientPlatform;
	}

	/**
	 * @param clientPlatform
	 */
	public void setClientPlatform(String clientPlatform) {
		this.clientPlatform = clientPlatform;
	}

	/**
	 * @return the technology filter string
	 */
	public String getTechnology() {
		return this.technology;
	}

	/**
	 * @param technology
	 */
	public void setTechnology(String technology) {
		this.technology = technology;
	}

	/**
	 * @return the server platform filter string
	 */
	public String getServerPlatform() {
		return this.serverPlatform;
	}

	/**
	 * @param serverPlatform
	 */
	public void setServerPlatform(String serverPlatform) {
		this.serverPlatform = serverPlatform;
	}

	/**
	 * @return the validation type filter string
	 */
	public String getValidationType() {
		return this.validationType;
	}

	/**
	 * @param validationType
	 */
	public void setValidationType(String validationType) {
		this.validationType = validationType;
	}

	/**
	 * @return the database filter string
	 */
	public String getDatabase() {
		return this.database;
	}

	/**
	 * @param database
	 */
	public void setDatabase(String database) {
		this.database = database;
	}

	/**
	 * @return the JPA provider filter string
	 */
	public String getJpaProvider() {
		return this.jpaProvider;
	}

	/**
	 * @param jpaProvider
	 */
	public void setJpaProvider(String jpaProvider) {
		this.jpaProvider = jpaProvider;
	}

	/**
	 * @return the description
	 */
	public String getDescription() {
		return this.description;
	}

	/**
	 * @param description
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * @return a list of dependencies that belong to this filter
	 */
	public List<DependencyMappingType> getDependencies() {
		return this.dependencies;
	}

	/**
	 * @param dependencies
	 */
	public void setDependencies(List<DependencyMappingType> dependencies) {
		this.dependencies = dependencies;
	}

}
