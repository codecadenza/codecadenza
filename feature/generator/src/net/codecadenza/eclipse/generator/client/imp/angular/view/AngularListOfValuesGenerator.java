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
package net.codecadenza.eclipse.generator.client.imp.angular.view;

import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for list-of-values dialogs of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularListOfValuesGenerator extends AbstractTypeScriptSourceGenerator {
	private final Form form;
	private final DTOBean dto;
	private final Project project;
	private final AngularI18NGenerator i18n;
	private final AngularCommonDataTableGenerator tableGenerator;

	/**
	 * Constructor
	 * @param form
	 */
	public AngularListOfValuesGenerator(Form form) {
		super(form.getTypeScriptSourceFile(), form.getTitle());

		this.form = form;
		this.dto = form.getDTO();
		this.project = dto.getNamespace().getProject();
		this.i18n = new AngularI18NGenerator(project);
		this.tableGenerator = new AngularCommonDataTableGenerator(this, form, i18n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importTypes(Stream.of("Component", "OnInit"), "@angular/core");
		importType("FieldTypeEnum", "../../common/model/field-type.enum");
		importType("TableDefinition", "../../common/model/table-definition.model");
		importType("AbstractListOfValuesDialog",
				"../../common/components/abstract-list-of-values-dialog/abstract-list-of-values-dialog");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		final var url = "templateUrl: '../../common/components/abstract-list-of-values-dialog/abstract-list-of-values-dialog.html'";

		var classDeclaration = "export class " + form.getName() + " extends AbstractListOfValuesDialog<";
		classDeclaration += dto.getName() + "> implements OnInit {";

		formatter.addLine("@Component({");
		formatter.increaseIndent();
		formatter.addLine("selector: '" + SELECTOR_PREFIX + form.getName().toLowerCase() + "',");
		formatter.addLine(url);
		formatter.decreaseIndent();
		formatter.addLine("})");
		formatter.addLine(classDeclaration);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addServiceOfSuperclass("ConfirmationService", "confirmationService", "primeng/api");
		addServiceOfSuperclass("MessageService", "messageService", "primeng/api");
		addServiceOfSuperclass("I18NService", "i18n", "../../common/services/i18n.service");
		addServiceOfSuperclass("FormatterService", "formatterService", "../../common/services/formatter.service");

		if (new AngularServiceInvocationGenerator(form.getBoundaryMethod(), dto).isAuthServiceRequired())
			addService("AuthService", "authService", "../../common/services/auth.service");

		addService(dto);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addMethods(AngularContentFormatter formatter) {
		final BoundaryMethod method = form.getBoundaryMethod();

		formatter.addBlockComment("Initialize the dialog");
		formatter.addLine("override ngOnInit() {");
		formatter.increaseIndent();
		formatter.addLine("console.log('Init dialog');");
		formatter.addBlankLine();
		formatter.addLine("this.init();");
		formatter.addLine("this.title = " + i18n.getI18N(form) + ";");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		tableGenerator.createTableSetup();

		formatter.addBlockComment("Load the items from the back-end");
		formatter.addLine("loadData(filterText: string) {");
		formatter.increaseIndent();
		formatter.addLine("return " + new AngularServiceInvocationGenerator(method, dto).createInvocation("filterText") + ";");
		formatter.decreaseIndent();
		formatter.addLine("}");

		i18n.save();
	}

}
