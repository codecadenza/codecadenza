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
package net.codecadenza.eclipse.diagram.domain.edit.parts;

import net.codecadenza.eclipse.diagram.domain.edit.helpers.DomainAssociationDecorationHelper;
import net.codecadenza.eclipse.diagram.domain.edit.policies.DomainObjectInheritanceItemSemanticEditPolicy;
import org.eclipse.draw2d.Connection;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ConnectionNodeEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ITreeBranchEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.EditPolicyRoles;
import org.eclipse.gmf.runtime.draw2d.ui.figures.PolylineConnectionEx;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Edit part for domain inheritance objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectInheritanceEditPart extends ConnectionNodeEditPart implements ITreeBranchEditPart {
	public static final int VISUAL_ID = 4005;

	/**
	 * @param view
	 */
	public DomainObjectInheritanceEditPart(View view) {
		super(view);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ConnectionNodeEditPart#createDefaultEditPolicies()
	 */
	@Override
	protected void createDefaultEditPolicies() {
		super.createDefaultEditPolicies();

		installEditPolicy(EditPolicyRoles.SEMANTIC_ROLE, new DomainObjectInheritanceItemSemanticEditPolicy());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ConnectionEditPart#createConnectionFigure()
	 */
	@Override
	protected Connection createConnectionFigure() {
		return new DomainObjectInheritanceFigure();
	}

	/**
	 * @return the figure
	 */
	public DomainObjectInheritanceFigure getPrimaryShape() {
		return (DomainObjectInheritanceFigure) getFigure();
	}

	public class DomainObjectInheritanceFigure extends PolylineConnectionEx {
		/**
		 * Constructor
		 */
		public DomainObjectInheritanceFigure() {
			setTargetDecoration(DomainAssociationDecorationHelper.createInheritanceDecoration());
		}
	}

}
