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
package net.codecadenza.eclipse.generator.client.imp.javafx.security;

import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import java.util.List;
import net.codecadenza.eclipse.generator.client.common.security.AbstractRichClientSecurityHelper;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;

/**
 * <p>
 * Security helper for JavaFX generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXSecurityHelper extends AbstractRichClientSecurityHelper {
	/**
	 * Constructor
	 * @param project
	 */
	public JavaFXSecurityHelper(Project project) {
		super(project);
	}

	/**
	 * @param roles
	 * @return the authorization check based on the given roles
	 */
	public String checkAuthorization(List<Role> roles) {
		final var b = new StringBuilder();
		final boolean hasRole = !roles.isEmpty();

		if (!addSecurity)
			return b.toString();

		boolean isFirstRole = true;

		if (hasRole) {
			for (final Role role : roles) {
				final var roleName = "ROLE_" + role.getName().toUpperCase();

				if (isFirstRole) {
					b.append("return " + SECURITY_MANAGER + ".checkAuthorization(" + securityServiceName + "." + roleName);

					isFirstRole = false;
				}
				else
					b.append(", " + securityServiceName + "." + roleName);
			}

			b.append(");\n");
		}
		else
			b.append("return false;\n");

		return b.toString();
	}

}
