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
package net.codecadenza.eclipse.generator.client.imp.angular.util;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;

/**
 * <p>
 * Utility class for generating a form's relative URL in an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularURLGenerator {
	/**
	 * Prevent instantiation
	 */
	private AngularURLGenerator() {

	}

	/**
	 * Create the relative URL for the given form
	 * @param form
	 * @param forRoute flag that controls if the URL should be used for a route definition
	 * @return the generated URL
	 */
	public static String createURL(Form form, boolean forRoute) {
		final DomainObject domainObject = form.getDomainObject();
		final var path = new StringBuilder();

		if (!forRoute)
			path.append("/");

		path.append(domainObject.getName().toLowerCase() + "/" + form.getName().toLowerCase());

		if (forRoute && (form.getFormType() == FormTypeEnumeration.ADD || form.getFormType() == FormTypeEnumeration.READONLY
				|| form.getFormType() == FormTypeEnumeration.UPDATE))
			path.append("/:id");

		return path.toString();
	}

}
