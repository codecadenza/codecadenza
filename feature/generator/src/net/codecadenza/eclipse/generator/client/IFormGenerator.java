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

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Form generator interface
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface IFormGenerator {
	/**
	 * Create or replace the source file of the application tree navigator
	 * @param project
	 * @throws Exception if the creation of the navigator source file has failed
	 */
	void createNavigator(Project project) throws Exception;

	/**
	 * Create or replace the source file of the given form
	 * @param form
	 * @throws Exception if the creation of the form source file has failed
	 */
	void createForm(Form form) throws Exception;

	/**
	 * Create or replace the source file of the given grid panel
	 * @param panel
	 * @throws Exception if the creation of the grid panel source file has failed
	 */
	void createGridPanel(FormPanel panel) throws Exception;
}
