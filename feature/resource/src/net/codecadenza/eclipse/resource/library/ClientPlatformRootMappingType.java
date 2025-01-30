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
package net.codecadenza.eclipse.resource.library;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * JAXB root mapping class for library configuration data
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlRootElement(name = "client_platforms")
@XmlAccessorType(XmlAccessType.FIELD)
public class ClientPlatformRootMappingType implements Serializable {
	private static final long serialVersionUID = 8954710712222772252L;

	@XmlElement(name = "client_platform", required = false)
	private List<ClientPlatformMappingType> clientPlatforms = new ArrayList<>();

	/**
	 * @return a list of elements
	 */
	public List<ClientPlatformMappingType> getClientPlatforms() {
		return this.clientPlatforms;
	}

	/**
	 * @param clientPlatforms
	 */
	public void setClientPlatforms(List<ClientPlatformMappingType> clientPlatforms) {
		this.clientPlatforms = clientPlatforms;
	}

}
