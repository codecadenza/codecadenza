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
package net.codecadenza.eclipse.generator.integration.method.imp.rest;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;

/**
 * <p>
 * Factory for REST integration method generators
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RESTMethodGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private RESTMethodGeneratorFactory() {

	}

	/**
	 * @param method
	 * @param parentGenerator
	 * @return the generator for the given REST integration method
	 */
	public static BasicRESTMethodGenerator getMethodGenerator(RESTIntegrationMethod method,
			AbstractJavaSourceGenerator parentGenerator) {
		final BoundaryMethodTypeEnumeration methodType = method.getBoundaryMethod().getMethodType();

		if (methodType == BoundaryMethodTypeEnumeration.SEARCH)
			return new SearchRESTMethodGenerator(method, parentGenerator);
		else if (methodType == BoundaryMethodTypeEnumeration.COUNT)
			return new CountRESTMethodGenerator(method, parentGenerator);

		return new BasicRESTMethodGenerator(method, parentGenerator);
	}

}
