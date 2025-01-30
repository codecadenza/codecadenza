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

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_COMMON_COMPONENTS_FOLDER;

import java.text.DecimalFormat;
import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.security.AngularSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularURLGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Generator for the application tree navigator of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularNavigatorGenerator extends AbstractTypeScriptSourceGenerator {
	private static DecimalFormat idFormat = new DecimalFormat("0000");

	private final Project project;
	private final AngularI18NGenerator i18n;
	private final AngularSecurityHelper securityHelper;
	private int nodeId = 1;

	/**
	 * Constructor
	 * @param project
	 */
	public AngularNavigatorGenerator(Project project) {
		super("This component displays all form groups and forms of the application in a tree view");

		this.project = project;
		this.i18n = new AngularI18NGenerator(project);
		this.securityHelper = new AngularSecurityHelper(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#getSourceFile()
	 */
	@Override
	public WorkspaceFile getSourceFile() {
		final var path = ANGULAR_COMMON_COMPONENTS_FOLDER + "/tree-navigator/tree-navigator.ts";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importTypes(Stream.of("Component", "OnInit", "Input"), "@angular/core");
		importType("TreeNode", "primeng/api");
		importType("TreeNavigatorNode", "../../model/tree-navigator-node.model");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		formatter.addLine("@Component({");
		formatter.increaseIndent();
		formatter.addLine("selector: '" + SELECTOR_PREFIX + "tree-navigator',");
		formatter.addLine("templateUrl: './tree-navigator.html'");
		formatter.decreaseIndent();
		formatter.addLine("})");
		formatter.addLine("export class TreeNavigator implements OnInit {");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addField(null, "FORM_GROUP_TYPE", "TreeNavigatorNode.FORM_GROUP_TYPE").create();
		addField(null, "FORM_TYPE", "TreeNavigatorNode.FORM_TYPE").create();
		addService("I18NService", "i18n", "../../services/i18n.service");
		addField("TreeNode[]", "nodes").withDefaultValue("[]").create();
		addField(null, "collapsed").withDefaultValue("false").withInputModifier().create();

		if (securityHelper.isSecurityEnabled()) {
			addService("AuthService", "authService", "../../services/auth.service");

			importType("RoleEnum", "../../model/role.enum");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addMethods(AngularContentFormatter formatter) {
		formatter.addBlockComment("Initialize the component");
		formatter.addLine("ngOnInit() {");
		formatter.increaseIndent();
		formatter.addLine("this.addNodes();");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		formatter.addBlockComment("Add all form group and form nodes to the tree view");
		formatter.addLine("addNodes() {");
		formatter.increaseIndent();

		boolean firstNode = true;

		// Add all top-level form groups
		for (final FormGroup group : project.getFormGroups()) {
			if (group.getParentGroup() != null)
				continue;

			final var nodeName = "folderNode" + idFormat.format(nodeId++);

			final var nodeFormatter = new AngularContentFormatter();
			nodeFormatter.addLine("const " + nodeName + " = new TreeNavigatorNode(" + i18n.getI18N(group) + ");");

			addGroupsToParent(group, nodeName, nodeFormatter);

			nodeFormatter.addBlankLine();
			nodeFormatter.addLine("this.nodes.push(" + nodeName + ");");

			if (firstNode)
				firstNode = false;
			else
				formatter.addBlankLine();

			securityHelper.wrapSecurityCode(formatter, group.getRoles(), nodeFormatter.getContent());
		}

		formatter.decreaseIndent();
		formatter.addLine("}");

		i18n.save();
	}

	/**
	 * Add groups and forms to the parent group
	 * @param parentGroup
	 * @param parentNodeName
	 * @param parentFormatter
	 */
	private void addGroupsToParent(FormGroup parentGroup, String parentNodeName, AngularContentFormatter parentFormatter) {
		// Add the form groups to the parent group
		parentGroup.getChildGroups().forEach(group -> {
			final var nodeName = "folderNode" + idFormat.format(nodeId++);
			final var nodeFormatter = new AngularContentFormatter(parentFormatter.getIndents());

			if (addChildNodes(group)) {
				nodeFormatter.addLine("const " + nodeName + " = " + parentNodeName + ".addFormGroupNode(" + i18n.getI18N(group) + ");");

				addGroupsToParent(group, nodeName, nodeFormatter);
			}
			else
				nodeFormatter.addLine(parentNodeName + ".addFormGroupNode('" + group.getName() + "');");

			parentFormatter.addBlankLine();

			securityHelper.wrapSecurityCode(parentFormatter, group.getRoles(), nodeFormatter.getContent());
		});

		// Add the forms of this group
		for (final Form form : parentGroup.getForms())
			if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW || form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW
					|| form.getFormType() == FormTypeEnumeration.TREE_VIEW) {
				String addFormNode = parentNodeName + ".addFormNode(" + i18n.getI18N(form);
				addFormNode += ", '" + AngularURLGenerator.createURL(form, false) + "');";

				final var nodeFormatter = new AngularContentFormatter(parentFormatter.getIndents());
				nodeFormatter.addLine(addFormNode);

				parentFormatter.addBlankLine();

				securityHelper.wrapSecurityCode(parentFormatter, form.getRoles(), nodeFormatter.getContent());
			}
	}

	/**
	 * @param formGroup
	 * @return true if at least one child node must be added to the given form group node
	 */
	private boolean addChildNodes(FormGroup formGroup) {
		if (!formGroup.getChildGroups().isEmpty())
			return true;

		for (final Form form : formGroup.getForms())
			if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW || form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW
					|| form.getFormType() == FormTypeEnumeration.TREE_VIEW)
				return true;

		return false;
	}

}
