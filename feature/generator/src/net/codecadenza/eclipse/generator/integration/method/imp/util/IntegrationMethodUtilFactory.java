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
package net.codecadenza.eclipse.generator.integration.method.imp.util;

import net.codecadenza.eclipse.generator.integration.method.imp.util.imp.CreateIntegrationMethodUtil;
import net.codecadenza.eclipse.generator.integration.method.imp.util.imp.ListIntegrationMethodUtil;
import net.codecadenza.eclipse.generator.integration.method.imp.util.imp.SaveIntegrationMethodUtil;
import net.codecadenza.eclipse.generator.integration.method.imp.util.imp.SingleObjectIntegrationMethodUtil;
import net.codecadenza.eclipse.generator.integration.method.imp.util.imp.UpdateIntegrationMethodUtil;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Factory for utilities that provide additional services for generating an integration method if the project is using facades
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationMethodUtilFactory {
	/**
	 * Prevent instantiation
	 */
	private IntegrationMethodUtilFactory() {

	}

	/**
	 * @param integrationMethod
	 * @return an utility for generating integration methods. It returns null if no utility for this method is available!
	 */
	public static AbstractIntegrationMethodUtil getInstance(AbstractIntegrationMethod integrationMethod) {
		final Project project = integrationMethod.getIntegrationBean().getNamespace().getProject();

		if (project.isBoundaryMode())
			return null;

		final BoundaryMethodTypeEnumeration type = integrationMethod.getBoundaryMethod().getMethodType();

		if (type == BoundaryMethodTypeEnumeration.FIND_BY_ID || type == BoundaryMethodTypeEnumeration.FIND_BY_OBJECT
				|| type == BoundaryMethodTypeEnumeration.FIND_EXISTING)
			return new SingleObjectIntegrationMethodUtil(integrationMethod);
		else if (type == BoundaryMethodTypeEnumeration.CREATE)
			return new CreateIntegrationMethodUtil(integrationMethod);
		else if (type == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER)
			return new ListIntegrationMethodUtil(integrationMethod);
		else if (type == BoundaryMethodTypeEnumeration.UPDATE)
			return new UpdateIntegrationMethodUtil(integrationMethod);
		else if (type == BoundaryMethodTypeEnumeration.SAVE)
			return new SaveIntegrationMethodUtil(integrationMethod);

		return null;
	}

}
