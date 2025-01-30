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
package net.codecadenza.eclipse.generator.client.common.security;

import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;

/**
 * <p>
 * Utility class for security-related source code generation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractRichClientSecurityHelper implements ISecurityHelper {
	protected boolean addSecurity;
	protected String securityServicePackage = "";
	protected String securityManagerPackage = "";
	protected String securityServiceName = "";

	/**
	 * Constructor
	 * @param project
	 */
	protected AbstractRichClientSecurityHelper(Project project) {
		final DTOBean logOnDTO = project.getApplicationLogOnDTO();
		final BoundaryBean logOnBoundary = project.getLogOnBoundary();

		if (logOnDTO != null && logOnBoundary != null) {
			securityServicePackage = logOnBoundary.getNamespace().toString() + ".*";
			securityManagerPackage = project.getClientNamespace().toString() + SUB_PACKAGE_UTIL + "." + SECURITY_MANAGER;
			securityServiceName = logOnBoundary.getInterfaceName();
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
					b.append("if(" + SECURITY_MANAGER + ".checkAuthorization(" + securityServiceName + "." + roleName);

					isFirstRole = false;
				}
				else
					b.append(", " + securityServiceName + "." + roleName);
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

		if (addSecurity) {
			imports.add("import " + securityServicePackage + ";");
			imports.add("import " + securityManagerPackage + ";");
		}

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
	 * @return the necessary security imports
	 */
	public String addSecurityImports() {
		final var b = new StringBuilder();

		if (addSecurity) {
			b.append("import " + securityServicePackage + ";\n");
			b.append("import " + securityManagerPackage + ";\n");
		}

		return b.toString();
	}

	/**
	 * @return all import statements for the security manager
	 */
	public Set<String> getSecurityManagerImports() {
		final var imports = new HashSet<String>();

		if (addSecurity)
			imports.add("import " + securityManagerPackage + ";");

		return imports;
	}

}
