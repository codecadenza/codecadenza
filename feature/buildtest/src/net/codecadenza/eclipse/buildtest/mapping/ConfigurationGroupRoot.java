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
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * JAXB root mapping class for configuration groups
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlRootElement(name = "config_groups")
@XmlAccessorType(XmlAccessType.FIELD)
public class ConfigurationGroupRoot implements Serializable {
	private static final long serialVersionUID = -4387134508906072233L;

	@XmlElement(name = "config_group", required = false)
	private List<ConfigurationGroup> configurationGroups = new ArrayList<>();

	/**
	 * @return a list of configuration group elements
	 */
	public List<ConfigurationGroup> getConfigurationGroups() {
		return this.configurationGroups;
	}

	/**
	 * @param configurationGroups
	 */
	public void setConfigurationGroups(List<ConfigurationGroup> configurationGroups) {
		this.configurationGroups = configurationGroups;
	}

}
