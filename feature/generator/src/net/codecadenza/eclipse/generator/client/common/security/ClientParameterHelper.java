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
package net.codecadenza.eclipse.generator.client.common.security;

import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Utility class for creating additional parameters in order to limit the number of returned records of respective boundary
 * methods by using either the ID of the logged on user or the ID of the client the user belongs to
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ClientParameterHelper {
	protected final Project project;
	protected final DTOBean logOnDTO;
	protected DTOBean listDTO;
	protected BoundaryMethod boundaryMethod;
	protected DTOBeanAttribute filterAttribute;

	/**
	 * Constructor
	 * @param listDTO
	 */
	public ClientParameterHelper(DTOBean listDTO) {
		this(listDTO.getNamespace().getProject());

		this.listDTO = listDTO;

		if (this.listDTO.getDomainObject().isMandated())
			this.filterAttribute = this.logOnDTO.getClientPKAttribute();
	}

	/**
	 * Constructor
	 * @param boundaryMethod
	 */
	public ClientParameterHelper(BoundaryMethod boundaryMethod) {
		this(boundaryMethod.getBoundaryBean().getNamespace().getProject());

		this.boundaryMethod = boundaryMethod;

		if (this.logOnDTO != null) {
			if (this.boundaryMethod.getDataFetchType() == BoundaryMethodDataFetchType.CLIENT)
				this.filterAttribute = this.logOnDTO.getClientPKAttribute();
			else if (this.boundaryMethod.getDataFetchType() == BoundaryMethodDataFetchType.USER)
				this.filterAttribute = this.logOnDTO.getPKAttribute();
		}
	}

	/**
	 * Constructor
	 * @param project
	 */
	protected ClientParameterHelper(Project project) {
		this.project = project;
		this.logOnDTO = project.getApplicationLogOnDTO();
	}

	/**
	 * @return the client parameter that is necessary to invoke the respective boundary method. An empty string is returned if the
	 *         method doesn't need an additional parameter!
	 */
	public String getClientParameter() {
		if (filterAttribute == null)
			return "";

		if (project.hasJSFClient())
			return "userSession.getPrincipal()." + filterAttribute.getGetterName();
		else if (project.hasVaadinClient())
			return MANAGED_SECURITY_MANAGER + ".getLogOnDTO()." + filterAttribute.getGetterName();
		else if (project.hasRAPClient() && boundaryMethod != null) {
			final BoundaryMethodTypeEnumeration methodType = boundaryMethod.getMethodType();
			final boolean isInvokedInNonUIThread = methodType == BoundaryMethodTypeEnumeration.SEARCH
					|| methodType == BoundaryMethodTypeEnumeration.COUNT;

			// In case of an Eclipse RAP application, the initialization of the parameter must be performed before invoking the boundary
			// method if it is called from a non-UI thread!
			if (isInvokedInNonUIThread)
				return filterAttribute.getName();
		}

		return SECURITY_MANAGER + ".getLogOnDTO()." + filterAttribute.getGetterName();
	}

	/**
	 * @return the filter attribute or null if no additional parameter is necessary
	 */
	public DTOBeanAttribute getClientFilterAttribute() {
		return filterAttribute;
	}

}
