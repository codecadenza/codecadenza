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
package net.codecadenza.eclipse.generator.client.common.navigator;

import java.text.DecimalFormat;
import net.codecadenza.eclipse.generator.client.common.security.ISecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Abstract base class for application tree navigator generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractNavigatorGenerator extends AbstractJavaSourceGenerator {
	protected final Project project;
	protected final ISecurityHelper securityHelper;
	protected final DecimalFormat indexFormat = new DecimalFormat("0000");
	protected final boolean hasTree;
	protected final boolean hasView;
	private int formGroupIndex = 1;

	/**
	 * Constructor
	 * @param project
	 * @param securityHelper
	 */
	protected AbstractNavigatorGenerator(Project project, ISecurityHelper securityHelper) {
		this.project = project;
		this.securityHelper = securityHelper;
		this.hasTree = project.getAllFormsOfProject().stream().map(Form::getFormType)
				.anyMatch(type -> type == FormTypeEnumeration.TREE_VIEW);
		this.hasView = project.getAllFormsOfProject().stream().map(Form::getFormType)
				.anyMatch(type -> type == FormTypeEnumeration.SEARCHABLE_VIEW || type == FormTypeEnumeration.SIMPLE_VIEW);
	}

	/**
	 * Add a form group to the parent node identified by the provided name
	 * @param parentNodeName
	 * @param nodeName
	 * @param group
	 * @return the generated content
	 */
	protected abstract String addFormGroup(String parentNodeName, String nodeName, FormGroup group);

	/**
	 * Add a view to the parent node identified by the provided name
	 * @param parentNodeName
	 * @param form
	 * @return the generated content
	 */
	protected abstract String addView(String parentNodeName, Form form);

	/**
	 * @return the name of the root node
	 */
	protected abstract String getRootNodeName();

	/**
	 * Create the navigator tree structure
	 * @return the generated content
	 */
	protected String createNavigatorTreeStructure() {
		final var b = new StringBuilder();

		// Add all top-level form groups
		for (final FormGroup group : project.getFormGroups()) {
			if (group.getParentGroup() != null)
				continue;

			b.append(addGroup(getRootNodeName(), group));
		}

		return b.toString();
	}

	/**
	 * Add groups and forms to the parent group
	 * @param parentNodeName
	 * @param parentGroup
	 * @return the generated content
	 */
	protected String addGroupsToParent(String parentNodeName, FormGroup parentGroup) {
		final var b = new StringBuilder();

		// Add the form groups to the parent group
		parentGroup.getChildGroups().forEach(group -> b.append(addGroup(parentNodeName, group)));

		// Add forms that belong to this group
		parentGroup.getForms().forEach(form -> {
			final FormTypeEnumeration formType = form.getFormType();

			if (formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW
					|| formType == FormTypeEnumeration.TREE_VIEW) {
				final var viewNode = addView(parentNodeName, form);

				b.append("\n");
				b.append(securityHelper.wrapSecurityCode(form.getRoles(), viewNode));
			}
		});

		return b.toString();
	}

	/**
	 * Add a form group to the parent node
	 * @param parentNodeName
	 * @param group
	 * @return the generated content
	 */
	private String addGroup(String parentNodeName, FormGroup group) {
		final var b = new StringBuilder();
		final var nodeName = "itemGroup" + indexFormat.format(formGroupIndex++);
		final String formGroupNode = addFormGroup(parentNodeName, nodeName, group);

		b.append("\n");
		b.append(securityHelper.wrapSecurityCode(group.getRoles(), formGroupNode));

		return b.toString();
	}

}
