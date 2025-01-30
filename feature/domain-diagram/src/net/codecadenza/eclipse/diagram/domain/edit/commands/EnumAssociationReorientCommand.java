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
package net.codecadenza.eclipse.diagram.domain.edit.commands;

import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.java.JavaEnum;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.common.core.command.CommandResult;
import org.eclipse.gmf.runtime.emf.type.core.commands.EditElementCommand;
import org.eclipse.gmf.runtime.emf.type.core.requests.ReorientRelationshipRequest;

/**
 * <p>
 * Command for re-orientation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EnumAssociationReorientCommand extends EditElementCommand {
	private final int reorientDirection;
	private final EObject oldEnd;
	private final EObject newEnd;

	/**
	 * @param request
	 */
	public EnumAssociationReorientCommand(ReorientRelationshipRequest request) {
		super(request.getLabel(), request.getRelationship(), request);

		reorientDirection = request.getDirection();
		oldEnd = request.getOldRelationshipEnd();
		newEnd = request.getNewRelationshipEnd();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.EditElementCommand#canExecute()
	 */
	@Override
	public boolean canExecute() {
		return false;
	}

	/**
	 * @return false as the re-orientation of associations is not supported
	 */
	protected boolean canReorientSource() {
		return false;
	}

	/**
	 * @return false as the re-orientation of associations is not supported
	 */
	protected boolean canReorientTarget() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand#
	 * doExecuteWithResult(org.eclipse.core.runtime.IProgressMonitor, org.eclipse.core.runtime.IAdaptable)
	 */
	@Override
	protected CommandResult doExecuteWithResult(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
		if (!canExecute())
			throw new ExecutionException("Invalid arguments in the reorient link command!");

		if (reorientDirection == ReorientRelationshipRequest.REORIENT_SOURCE)
			return reorientSource();

		if (reorientDirection == ReorientRelationshipRequest.REORIENT_TARGET)
			return reorientTarget();

		throw new IllegalStateException();
	}

	/**
	 * @return the source of the re-orientation
	 */
	protected CommandResult reorientSource() {
		getLink().setSource(getNewSource());
		return CommandResult.newOKCommandResult(getLink());
	}

	/**
	 * @return the target of the re-orientation
	 */
	protected CommandResult reorientTarget() {
		getLink().setTarget(getNewTarget());
		return CommandResult.newOKCommandResult(getLink());
	}

	/**
	 * @return the link
	 */
	protected EnumAssociation getLink() {
		return (EnumAssociation) getElementToEdit();
	}

	/**
	 * @return the old source
	 */
	protected DomainObject getOldSource() {
		return (DomainObject) oldEnd;
	}

	/**
	 * @return the new source
	 */
	protected DomainObject getNewSource() {
		return (DomainObject) newEnd;
	}

	/**
	 * @return the old target
	 */
	protected JavaEnum getOldTarget() {
		return (JavaEnum) oldEnd;
	}

	/**
	 * @return the new target
	 */
	protected JavaEnum getNewTarget() {
		return (JavaEnum) newEnd;
	}

}
