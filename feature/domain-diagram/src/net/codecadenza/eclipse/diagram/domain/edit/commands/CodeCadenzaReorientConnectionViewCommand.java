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

import java.util.List;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gmf.runtime.common.core.command.CommandResult;
import org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand;
import org.eclipse.gmf.runtime.notation.Edge;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Command to re-orient a connection
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaReorientConnectionViewCommand extends AbstractTransactionalCommand {
	private IAdaptable edgeAdaptor;

	/**
	 * @param editingDomain
	 * @param label
	 */
	public CodeCadenzaReorientConnectionViewCommand(TransactionalEditingDomain editingDomain, String label) {
		super(editingDomain, label, null);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand#getAffectedFiles()
	 */
	@Override
	public List<IFile> getAffectedFiles() {
		final View view = edgeAdaptor.getAdapter(View.class);

		if (view != null)
			return getWorkspaceFiles(view);

		return super.getAffectedFiles();
	}

	/**
	 * @return the edge adaptor
	 */
	public IAdaptable getEdgeAdaptor() {
		return edgeAdaptor;
	}

	/**
	 * @param edgeAdaptor
	 */
	public void setEdgeAdaptor(IAdaptable edgeAdaptor) {
		this.edgeAdaptor = edgeAdaptor;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand#
	 * doExecuteWithResult(org.eclipse.core.runtime.IProgressMonitor, org.eclipse.core.runtime.IAdaptable)
	 */
	@Override
	protected CommandResult doExecuteWithResult(IProgressMonitor progressMonitor, IAdaptable info) {
		if (edgeAdaptor == null)
			throw new IllegalStateException("Field 'edgeAdaptor' was not initialized!");

		final Edge edge = getEdgeAdaptor().getAdapter(Edge.class);

		if (edge == null)
			throw new IllegalStateException("Variable 'edge' must not be null!");

		final View tempView = edge.getSource();
		edge.setSource(edge.getTarget());
		edge.setTarget(tempView);

		return CommandResult.newOKCommandResult();
	}

}
