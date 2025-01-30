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
package net.codecadenza.eclipse.generator.client.imp.angular.module;

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_PAGE_FOLDER;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.security.AngularSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularURLGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Generator for modules that contain all forms of a specific domain object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularFormModuleGenerator extends AbstractTypeScriptSourceGenerator {
	private final DomainObject domainObject;
	private final Project project;
	private final AngularSecurityHelper securityHelper;
	private final List<Form> forms;
	private final List<FormPanel> gridPanels;

	/**
	 * Constructor
	 * @param domainObject
	 */
	public AngularFormModuleGenerator(DomainObject domainObject) {
		super("Module for forms of domain object " + domainObject.getLabel());

		this.domainObject = domainObject;
		this.project = domainObject.getNamespace().getProject();
		this.securityHelper = new AngularSecurityHelper(project);
		this.forms = project.getAllFormsOfProject().stream().filter(f -> f.getDomainObject().equals(domainObject)).toList();
		this.gridPanels = project.getAllGridPanelsOfProject().stream().filter(g -> g.getDTO().getDomainObject().equals(domainObject))
				.toList();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#getSourceFile()
	 */
	@Override
	public WorkspaceFile getSourceFile() {
		final String domainObjectName = domainObject.getName().toLowerCase();
		final var path = ANGULAR_PAGE_FOLDER + "/" + domainObjectName + "/" + domainObjectName + ".module.ts";

		return new WorkspaceFile(domainObject.getNamespace().getProject(), BuildArtifactType.GUI, path, null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		boolean importAuthGuardService = false;

		importType("NgModule", "@angular/core");
		importTypes(Stream.of("RouterModule", "Routes"), "@angular/router");
		importType("AppCommonModule", "../../common/app-common.module");

		for (final Form form : forms) {
			if (securityHelper.isSecurityEnabled() && form.getFormType() != FormTypeEnumeration.LOV)
				importAuthGuardService = true;

			importType(form.getName(), "./" + form.getName().toLowerCase());
		}

		gridPanels.forEach(gridPanel -> importType(gridPanel.getName(), "./" + gridPanel.getName().toLowerCase()));

		if (importAuthGuardService) {
			importType("AuthGuardService", "../../common/services/auth-guard.service");
			importType("RoleEnum ", "../../common/model/role.enum");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		formatter.addLine("const formRoutes: Routes = [");
		formatter.increaseIndent();

		for (final Form form : forms) {
			if (form.getFormType() == FormTypeEnumeration.LOV)
				continue;

			formatter.addLine("{");
			formatter.increaseIndent();
			formatter.addLine("path: '" + AngularURLGenerator.createURL(form, true) + "',");

			if (securityHelper.isSecurityEnabled()) {
				final Optional<String> roleList = form.getRoles().stream().map(r -> "RoleEnum." + r.getName())
						.reduce((r1, r2) -> r1 + ", " + r2);

				formatter.addLine("component: " + form.getName() + ",");
				formatter.addLine("canActivate: [AuthGuardService],");
				formatter.addLine("data: { roles: [" + roleList.orElse("") + "] }");
			}
			else
				formatter.addLine("component: " + form.getName());

			formatter.decreaseIndent();
			formatter.addLine("},");
		}

		formatter.decreaseIndent();
		formatter.addLine("];");
		formatter.addBlankLine();
		formatter.addLine("@NgModule({");
		formatter.increaseIndent();
		formatter.addLine("declarations: [");
		formatter.increaseIndent();

		forms.forEach(form -> formatter.addLine(form.getName() + ","));

		gridPanels.forEach(gridPanel -> formatter.addLine(gridPanel.getName() + ","));

		formatter.decreaseIndent();
		formatter.addLine("],");
		formatter.addLine("exports: [");
		formatter.increaseIndent();

		for (final Form form : forms) {
			if (form.getFormType() != FormTypeEnumeration.LOV)
				continue;

			formatter.addLine(form.getName() + ",");
		}

		gridPanels.forEach(gridPanel -> formatter.addLine(gridPanel.getName() + ","));

		formatter.decreaseIndent();
		formatter.addLine("],");
		formatter.addLine("imports: [");
		formatter.increaseIndent();
		formatter.addLine("AppCommonModule,");

		final var refModules = new HashSet<DomainObject>();
		refModules.add(domainObject);

		final var appModules = new StringBuilder();

		// Search for list-of-values dialogs of other modules that must be imported
		for (final Form form : forms)
			for (final FormField formField : form.getAllFormFields()) {
				if (!formField.isVisible() || formField.isReadonly() || formField.getFieldType() != FormFieldTypeEnumeration.LOV)
					continue;

				if (!formField.getDTOAttribute().getDTOBean().equals(form.getDTO()))
					continue;

				if (!refModules.contains(formField.getListOfValues().getDomainObject())) {
					final String domainObjName = formField.getListOfValues().getDomainObject().getName();

					appModules.append(domainObjName + "Module, ");
					refModules.add(formField.getListOfValues().getDomainObject());

					importType(domainObjName + "Module",
							"../" + domainObjName.toLowerCase() + "/" + domainObjName.toLowerCase() + ".module");
				}
			}

		// Search for grid panels of other modules that must be imported
		for (final Form form : forms)
			for (final FormPanel formPanel : form.getFormPanels()) {
				if (formPanel.getBasePanel() == null)
					continue;

				final DTOBean panelDTO = formPanel.getBasePanel().getDTO();

				if (!refModules.contains(panelDTO.getDomainObject())) {
					final String domainObjName = panelDTO.getDomainObject().getName();

					appModules.append(domainObjName + "Module, ");
					refModules.add(panelDTO.getDomainObject());

					importType(domainObjName + "Module",
							"../" + domainObjName.toLowerCase() + "/" + domainObjName.toLowerCase() + ".module");
				}
			}

		if (!appModules.toString().isEmpty())
			formatter.addLine(appModules.toString().trim());

		formatter.addLine("RouterModule.forChild(formRoutes)");
		formatter.decreaseIndent();
		formatter.addLine("]");
		formatter.decreaseIndent();
		formatter.addLine("})");
		formatter.addLine("export class " + domainObject.getName() + "Module {");
	}

}
