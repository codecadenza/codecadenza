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
package net.codecadenza.eclipse.generator.client.imp.vaadin;

import net.codecadenza.eclipse.generator.client.IFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.form.VaadinGridPanelGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.form.VaadinSingleRecordFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.view.VaadinListOfValuesGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.view.VaadinNavigatorGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.view.VaadinSavedQueryViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.view.VaadinTreeViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.view.VaadinViewGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for forms of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinFormGenerator implements IFormGenerator {
	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.IFormGenerator#createNavigator(net.codecadenza.eclipse.model.project.Project)
	 */
	@Override
	public void createNavigator(Project project) throws Exception {
		new VaadinNavigatorGenerator(project).createSourceFile();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.IFormGenerator#createGridPanel(net.codecadenza.eclipse.model.client.FormPanel)
	 */
	@Override
	public void createGridPanel(FormPanel panel) throws Exception {
		new VaadinGridPanelGenerator(panel).createSourceFile();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.IFormGenerator#createForm(net.codecadenza.eclipse.model.client.Form)
	 */
	@Override
	public void createForm(Form form) throws Exception {
		final FormTypeEnumeration formType = form.getFormType();
		final Project project = form.getDomainObject().getNamespace().getProject();

		if (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.UPDATE
				|| form.getFormType() == FormTypeEnumeration.READONLY) {
			// Rearrange all fields in order to avoid problems due to inappropriate layout settings!
			form.rearrangeFields();

			new VaadinSingleRecordFormGenerator(form).createSourceFile();
			return;
		}

		if (formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW)
			new VaadinViewGenerator(form).createSourceFile();
		else if (formType == FormTypeEnumeration.LOV)
			new VaadinListOfValuesGenerator(form).createSourceFile();
		else if (formType == FormTypeEnumeration.TREE_VIEW)
			new VaadinTreeViewGenerator((TreeView) form).createSourceFile();

		if (formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW
				|| formType == FormTypeEnumeration.TREE_VIEW) {
			// Create the application tree navigator
			createNavigator(project);
		}

		if ((formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.TREE_VIEW)
				&& project.getApplicationLogOnDTO() != null && project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY) != null)
			new VaadinSavedQueryViewGenerator(project).createSourceFile();
	}

}
