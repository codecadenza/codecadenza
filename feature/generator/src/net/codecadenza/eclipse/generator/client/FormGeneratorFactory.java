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
package net.codecadenza.eclipse.generator.client;

import net.codecadenza.eclipse.generator.client.imp.angular.AngularFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.EclipseFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.JavaFXFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.SwingFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinFormGenerator;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Factory for client form generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormGeneratorFactory {
	/**
	 * Private constructor
	 */
	private FormGeneratorFactory() {

	}

	/**
	 * Factory method
	 * @param project
	 * @return a form generator based on the project's client platform
	 * @throws IllegalStateException if an implementation for the given client platform is not available
	 */
	public static IFormGenerator getFormGenerator(Project project) {
		if (project.hasEclipseClient())
			return new EclipseFormGenerator();
		else if (project.hasSwingClient())
			return new SwingFormGenerator();
		else if (project.hasJSFClient())
			return new JSFFormGenerator();
		else if (project.hasVaadinClient())
			return new VaadinFormGenerator();
		else if (project.hasJavaFXClient())
			return new JavaFXFormGenerator();
		else if (project.hasAngularClient())
			return new AngularFormGenerator();

		throw new IllegalStateException(
				"A form generator for the client platform '" + project.getClientPlatform() + "' is not available!");
	}

}
