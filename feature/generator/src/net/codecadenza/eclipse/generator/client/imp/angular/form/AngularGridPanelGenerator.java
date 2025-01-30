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
package net.codecadenza.eclipse.generator.client.imp.angular.form;

import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.STRING;

import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for grid panels of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularGridPanelGenerator extends AbstractTypeScriptSourceGenerator {
	private static final String PARENT_OBJ_ID = "parentObjectId";

	private final FormPanel gridPanel;
	private final DomainObject domainObject;
	private final Project project;
	private final AngularI18NGenerator i18n;
	private final AngularCommonDataTableGenerator tableGenerator;

	/**
	 * Constructor
	 * @param gridPanel
	 */
	public AngularGridPanelGenerator(FormPanel gridPanel) {
		super(gridPanel.getTypeScriptSourceFile(), gridPanel.getLabel());

		this.gridPanel = gridPanel;
		this.domainObject = gridPanel.getDTO().getDomainObject();
		this.project = domainObject.getNamespace().getProject();
		this.i18n = new AngularI18NGenerator(project);
		this.tableGenerator = new AngularCommonDataTableGenerator(this, gridPanel, i18n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importTypes(Stream.of("Component", "Input", "OnInit"), "@angular/core");
		importType("TableDefinition", "../../common/model/table-definition.model");
		importType("AbstractDataTable", "../../common/components/abstract-data-table/abstract-data-table");

		tableGenerator.addImports();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		var classDeclaration = "export class " + gridPanel.getName() + " extends AbstractDataTable<";
		classDeclaration += gridPanel.getDTO().getName() + "> implements OnInit {";

		formatter.addLine("@Component({");
		formatter.increaseIndent();
		formatter.addLine("selector: '" + SELECTOR_PREFIX + gridPanel.getName().toLowerCase() + "',");
		formatter.addLine("templateUrl: '../../common/components/abstract-data-table/abstract-data-table.html'");
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
		final DomainObject boundaryDomainObject = gridPanel.getBoundaryMethod().getBoundaryBean().getDomainObject();

		addService(boundaryDomainObject);
		addServiceOfSuperclass("ConfirmationService", "confirmationService", "primeng/api");
		addServiceOfSuperclass("MessageService", "messageService", "primeng/api");
		addServiceOfSuperclass("I18NService", "i18n", "../../common/services/i18n.service");
		addServiceOfSuperclass("FormatterService", "formatterService", "../../common/services/formatter.service");
		addServiceOfSuperclass("WindowResizeEventController", "windowEventController",
				"../../common/listeners/window-resize-event-controller.service");

		addField(null, PARENT_OBJ_ID).withDefaultValue("''").withInputModifier().create();

		if (gridPanel.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
			addField(null, "readonly").withDefaultValue("false").withInputModifier().create();

		tableGenerator.addFields();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addMethods(AngularContentFormatter formatter) {
		final String invocation = new AngularServiceInvocationGenerator(gridPanel.getBoundaryMethod())
				.createInvocation(PARENT_OBJ_ID);

		formatter.addBlockComment("Initialize the dialog");
		formatter.addLine("ngOnInit() {");
		formatter.increaseIndent();
		formatter.addLine("this.init();");

		if (tableGenerator.showButtonForCreatingObject()) {
			formatter.addBlankLine();
			formatter.addIfStatement("!this.readonly", "this.showNewButton = true;", false);
		}

		if (tableGenerator.showButtonForDataImport()) {
			formatter.addBlankLine();
			formatter.addIfStatement("!this.readonly", "this.showImportButton = true;", false);
		}

		tableGenerator.addContextMenuItems();

		formatter.addBlankLine();
		formatter.addLine("this.refreshView();");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		tableGenerator.createTableSetup();

		formatter.addBlockComment("Load the items from the back-end");
		formatter.addLine("loadData(" + PARENT_OBJ_ID + ": " + STRING + ") {");
		formatter.increaseIndent();
		formatter.addLine("return " + invocation + ";");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		formatter.addBlockComment("Reload the data from the back-end and display the objects in the table");
		formatter.addLine("refreshView() {");
		formatter.increaseIndent();
		formatter.addLine("this.searchItems(this." + PARENT_OBJ_ID + ");");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		tableGenerator.createDoubleClickHandler();

		tableGenerator.createActionMethods();

		i18n.save();
	}

}
