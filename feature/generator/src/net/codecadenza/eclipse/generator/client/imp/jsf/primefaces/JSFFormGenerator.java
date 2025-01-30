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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces;

import net.codecadenza.eclipse.generator.client.IFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.converter.JSFConverterGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.JSFGridPanelGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.JSFSingleRecordFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFEnumConverterGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.view.JSFListOfValuesGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.view.JSFNavigatorGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.view.JSFTreeViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.view.JSFViewGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Generator for forms of a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFFormGenerator implements IFormGenerator {
	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.IFormGenerator#createNavigator(net.codecadenza.eclipse.model.project.Project)
	 */
	@Override
	public void createNavigator(Project project) throws Exception {
		new JSFNavigatorGenerator(project).createSourceFile();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.IFormGenerator#createGridPanel(net.codecadenza.eclipse.model.client.FormPanel)
	 */
	@Override
	public void createGridPanel(FormPanel panel) throws Exception {
		final var gridPanelGenerator = new JSFGridPanelGenerator(panel);
		final String pageContent = gridPanelGenerator.createXHTMLForm();

		// Create the source file
		gridPanelGenerator.createSourceFile();

		final WorkspaceFile uiFile = panel.getUserInterfaceFile();
		uiFile.setContent(pageContent);

		// Create the facelet file
		EclipseIDEService.createOrUpdateFile(uiFile);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.IFormGenerator#createForm(net.codecadenza.eclipse.model.client.Form)
	 */
	@Override
	public void createForm(Form form) throws Exception {
		final Project project = form.getDomainObject().getNamespace().getProject();
		final FormTypeEnumeration ft = form.getFormType();
		var pageContent = "";

		if (ft == FormTypeEnumeration.SIMPLE_VIEW || ft == FormTypeEnumeration.SEARCHABLE_VIEW) {
			final var viewGenerator = new JSFViewGenerator(form);
			viewGenerator.createSourceFile();

			pageContent = viewGenerator.createXHTMLForm();
		}
		else if (ft == FormTypeEnumeration.TREE_VIEW) {
			final var treeGenerator = new JSFTreeViewGenerator((TreeView) form);
			treeGenerator.createSourceFile();

			pageContent = treeGenerator.createXHTMLForm();
		}
		else if (ft == FormTypeEnumeration.LOV) {
			final var listOfValuesGenerator = new JSFListOfValuesGenerator(form);
			listOfValuesGenerator.createSourceFile();

			pageContent = listOfValuesGenerator.createXHTMLForm();
		}
		else if (ft == FormTypeEnumeration.CREATE || ft == FormTypeEnumeration.READONLY || ft == FormTypeEnumeration.UPDATE
				|| ft == FormTypeEnumeration.ADD) {
			// Rearrange all fields in order to avoid problems due to inappropriate layout settings!
			form.rearrangeFields();

			final var formGenerator = new JSFSingleRecordFormGenerator(form);
			formGenerator.createSourceFile();

			pageContent = formGenerator.createXHTMLForm();

			// Search for all list DTOs and create the corresponding converters
			for (final FormField f : form.getAllFormFields()) {
				final DTOBean refDTO = f.getDTOAttribute().getReferencedDTOBean();

				if (refDTO == null)
					continue;

				new JSFConverterGenerator(refDTO).createSourceFile();
			}

			// Search for all enumeration fields and create the corresponding converters
			for (final FormField f : form.getAllFormFields()) {
				if (!f.isVisible() || f.getFieldType() != FormFieldTypeEnumeration.ENUM_COMBOBOX)
					continue;

				final var javaEnum = (JavaEnum) f.getDTOAttribute().getDomainAttribute().getJavaType();

				new JSFEnumConverterGenerator(javaEnum).createSourceFile();
			}
		}

		final WorkspaceFile faceletFile = form.getUserInterfaceFile();
		faceletFile.setContent(pageContent);

		// Create the facelet file
		EclipseIDEService.createOrUpdateFile(faceletFile);

		if (ft == FormTypeEnumeration.SIMPLE_VIEW || ft == FormTypeEnumeration.SEARCHABLE_VIEW
				|| ft == FormTypeEnumeration.TREE_VIEW) {
			// Create the application tree navigator
			createNavigator(project);
		}
	}

}
