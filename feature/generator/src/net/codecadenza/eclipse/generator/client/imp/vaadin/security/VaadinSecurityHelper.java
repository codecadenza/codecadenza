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
package net.codecadenza.eclipse.generator.client.imp.vaadin.security;

import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.HOME_VIEW;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.generator.client.common.security.ISecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;

/**
 * <p>
 * Security helper for Vaadin generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinSecurityHelper implements ISecurityHelper {
	private boolean addSecurity;
	private String securityManagerPackage = "";

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinSecurityHelper(Project project) {
		final DTOBean logOnDTO = project.getApplicationLogOnDTO();
		final BoundaryBean logOnBoundary = project.getLogOnBoundary();

		if (logOnDTO != null && logOnBoundary != null) {
			securityManagerPackage = project.getClientNamespace().toString() + SUB_PACKAGE_UTIL + "." + SECURITY_MANAGER;
			addSecurity = true;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.security.ISecurityHelper#wrapSecurityCode(java.util.List,
	 * java.lang.String)
	 */
	@Override
	public String wrapSecurityCode(List<Role> roles, String source) {
		final var b = new StringBuilder();
		final boolean hasRole = !roles.isEmpty();
		final boolean addBlock = source.length() - source.replace(";", "").length() > 1;

		if (addSecurity && hasRole) {
			boolean isFirstRole = true;

			for (final Role role : roles) {
				final var roleName = "ROLE_" + role.getName().toUpperCase();

				if (isFirstRole) {
					b.append("if(" + MANAGED_SECURITY_MANAGER + ".checkAuthorization(");

					isFirstRole = false;
				}
				else
					b.append(", ");

				b.append(SECURITY_MANAGER + "." + roleName);
			}

			b.append("))\n");

			if (addBlock)
				b.append("{\n");
		}

		if ((addSecurity && hasRole) || !addSecurity)
			b.append(source);

		if (addSecurity && hasRole && addBlock)
			b.append("}\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.security.ISecurityHelper#getSecurityImports()
	 */
	@Override
	public Set<String> getSecurityImports() {
		final var imports = new HashSet<String>();

		if (addSecurity)
			imports.add("import " + securityManagerPackage + ";");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.security.ISecurityHelper#isSecurityAdded()
	 */
	@Override
	public boolean isSecurityAdded() {
		return addSecurity;
	}

	/**
	 * @param i18n
	 * @param roles
	 * @return the fragment that contains a permission check based on the provided roles
	 */
	public String addFormPermissionCheck(VaadinI18NGenerator i18n, List<Role> roles) {
		final var b = new StringBuilder();
		final boolean hasRole = !roles.isEmpty();

		if (!addSecurity || !hasRole)
			return b.toString();

		boolean isFirstRole = true;

		for (final Role role : roles) {
			final var roleName = "ROLE_" + role.getName().toUpperCase();

			if (isFirstRole) {
				b.append("if(!" + MANAGED_SECURITY_MANAGER + ".checkAuthorization(");

				isFirstRole = false;
			}
			else
				b.append(", ");

			b.append(SECURITY_MANAGER + "." + roleName);
		}

		final var dialogMessage = i18n.getI18NMessage("msg_err_permission",
				"You are not authoritzed to navigate to the requested page!");

		b.append("))\n");
		b.append("{\n");
		b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_permission", "Failed permission check") + ";\n");
		b.append("final String dialogMessage = " + dialogMessage + ";\n\n");
		b.append("new WarningMessageDialog(dialogTitle, dialogMessage, " + i18n.getLocaleFragment() + ").open();\n");
		b.append("event.forwardTo(" + HOME_VIEW + ".class);");
		b.append("return;");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return the necessary security imports
	 */
	public String addSecurityImports() {
		final var b = new StringBuilder();

		if (addSecurity)
			b.append("import " + securityManagerPackage + ";\n");

		return b.toString();
	}

}
