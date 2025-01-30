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
package net.codecadenza.eclipse.generator.client.imp.angular;

import net.codecadenza.eclipse.generator.client.IFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.domain.AngularDomainObjectGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.domain.AngularEnumGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.form.AngularGridPanelGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.form.AngularSingleRecordFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.module.AngularAppModuleGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.module.AngularFormModuleGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.view.AngularListOfValuesGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.view.AngularNavigatorGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.view.AngularTreeViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.view.AngularViewGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.Constants;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Generator for forms of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularFormGenerator implements IFormGenerator {
	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.IFormGenerator#createNavigator(net.codecadenza.eclipse.model.project.Project)
	 */
	@Override
	public void createNavigator(Project project) throws Exception {
		new AngularNavigatorGenerator(project).createSourceFile();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.IFormGenerator#createGridPanel(net.codecadenza.eclipse.model.client.FormPanel)
	 */
	@Override
	public void createGridPanel(FormPanel panel) throws Exception {
		final DTOBean dto = panel.getDTO();
		final Project project = dto.getNamespace().getProject();
		final AbstractDomainAssociation assoc = panel.getAssociation();
		final BoundaryBean parentBoundaryBean = project.getBoundaryByDomainObject(assoc.getDomainObject());
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(dto.getDomainObject());

		createModules(dto);

		new AngularServiceGenerator(parentBoundaryBean).createSourceFile();
		new AngularDomainObjectGenerator(dto).createSourceFile();

		if (boundaryBean != null)
			new AngularServiceGenerator(boundaryBean).createSourceFile();

		final var generator = new AngularGridPanelGenerator(panel);
		generator.createSourceFile();

		for (final JavaEnum javaEnum : generator.getDependentEnums())
			new AngularEnumGenerator(javaEnum).createSourceFile();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.IFormGenerator#createForm(net.codecadenza.eclipse.model.client.Form)
	 */
	@Override
	public void createForm(Form form) throws Exception {
		final FormTypeEnumeration formType = form.getFormType();
		final DTOBean dto = form.getDTO();
		final Project project = dto.getNamespace().getProject();
		final AbstractTypeScriptSourceGenerator generator;

		createModules(dto);

		if (formType == FormTypeEnumeration.LOV)
			generator = new AngularListOfValuesGenerator(form);
		else if (formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW)
			generator = new AngularViewGenerator(form);
		else if (formType == FormTypeEnumeration.TREE_VIEW) {
			generator = new AngularTreeViewGenerator((TreeView) form);

			new AngularTreeViewGenerator((TreeView) form).createTemplateFile();
		}
		else {
			generator = new AngularSingleRecordFormGenerator(form);

			new AngularSingleRecordFormGenerator(form).createTemplateFile();
		}

		generator.createSourceFile();

		// Create all necessary services and domain objects
		for (final DTOBean dependentDTO : generator.getDependentDTOs()) {
			new AngularDomainObjectGenerator(dependentDTO).createSourceFile();
			new AngularServiceGenerator(project.getBoundaryByDomainObject(dependentDTO.getDomainObject())).createSourceFile();
		}

		for (final JavaEnum javaEnum : generator.getDependentEnums())
			new AngularEnumGenerator(javaEnum).createSourceFile();

		if (formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW
				|| formType == FormTypeEnumeration.TREE_VIEW)
			createNavigator(form.getDomainObject().getNamespace().getProject());
	}

	/**
	 * Create the application module and the respective form module
	 * @param dto
	 * @throws Exception if an internal error has occurred
	 */
	private void createModules(DTOBean dto) throws Exception {
		final Project project = dto.getNamespace().getProject();
		final var pageFolder = Constants.ANGULAR_PAGE_FOLDER + "/" + dto.getDomainObject().getName().toLowerCase();

		// Create the folder where the forms of this domain object should be saved to
		EclipseIDEService.createFolder(project.getTargetProjectName(BuildArtifactType.GUI), pageFolder);

		new AngularAppModuleGenerator(project).createSourceFile();
		new AngularFormModuleGenerator(dto.getDomainObject()).createSourceFile();
	}

}
