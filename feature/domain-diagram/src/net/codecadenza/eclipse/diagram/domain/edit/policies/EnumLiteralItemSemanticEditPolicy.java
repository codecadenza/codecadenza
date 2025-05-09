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
package net.codecadenza.eclipse.diagram.domain.edit.policies;

import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.gmf.runtime.emf.type.core.commands.DestroyElementCommand;
import org.eclipse.gmf.runtime.emf.type.core.requests.DestroyElementRequest;

/**
 * <p>
 * Policy class
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EnumLiteralItemSemanticEditPolicy extends CodeCadenzaBaseItemSemanticEditPolicy {
	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaBaseItemSemanticEditPolicy#
	 * getDestroyElementCommand(org.eclipse.gmf.runtime.emf.type.core.requests.DestroyElementRequest)
	 */
	@Override
	protected Command getDestroyElementCommand(DestroyElementRequest req) {
		final CompoundCommand cc = getDestroyEdgesCommand();

		addDestroyShortcutsCommand(cc);

		cc.add(getGEFWrapper(new DestroyElementCommand(req)));

		return cc.unwrap();
	}

}
