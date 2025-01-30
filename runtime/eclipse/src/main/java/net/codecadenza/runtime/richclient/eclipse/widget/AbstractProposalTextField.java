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
package net.codecadenza.runtime.richclient.eclipse.widget;

import java.util.Collection;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.fieldassist.TextContentAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Abstract proposal text field
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the proposal text field
 */
public abstract class AbstractProposalTextField<T> implements IContentProposalProvider {
	private static final int DEFAULT_INPUT_LENGTH = 2;

	private Text internalTextField;
	private T selectedItem;
	private int filterLength = DEFAULT_INPUT_LENGTH;
	private ContentProposalAdapter proposalAdapter;

	/**
	 * @param parent
	 * @param style
	 * @param filterLength number of characters that must be inserted into field before proposal data is fetched
	 */
	protected AbstractProposalTextField(Composite parent, int style, int filterLength) {
		this(parent, style);

		if (filterLength < 1)
			this.filterLength = 1;
		else
			this.filterLength = filterLength;
	}

	/**
	 * Constructor
	 * @param parent
	 * @param style
	 */
	protected AbstractProposalTextField(Composite parent, int style) {
		this.internalTextField = new Text(parent, style);

		proposalAdapter = new ContentProposalAdapter(internalTextField, new TextContentAdapter(), this, null, null);
		proposalAdapter.setProposalAcceptanceStyle(ContentProposalAdapter.PROPOSAL_REPLACE);

		proposalAdapter.addContentProposalListener(proposal -> {
			@SuppressWarnings("unchecked")
			final var prop = (ContentProposal<T>) proposal;

			selectedItem = prop.element;

			onProposalAccepted(selectedItem);
		});

		internalTextField.addKeyListener(new KeyListener() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent event) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent event) {
				// Clear the selection as soon as user presses any key except for SWT.CR!
				if (event.character != SWT.CR)
					selectedItem = null;
			}
		});
	}

	/**
	 * @return the proposal adapter
	 */
	public ContentProposalAdapter getProposalAdapter() {
		return proposalAdapter;
	}

	/**
	 * @return the text control
	 */
	public Text getControl() {
		return internalTextField;
	}

	/**
	 * @return the selected item
	 */
	public T getSelectedItem() {
		return selectedItem;
	}

	/**
	 * Set the selected item
	 * @param element
	 */
	public void setSelectedItem(T element) {
		selectedItem = element;

		if (element != null)
			internalTextField.setText(getProposalLabel(element));
		else
			internalTextField.setText("");
	}

	/**
	 * Request the keyboard focus for this component
	 */
	public void setFocus() {
		internalTextField.setFocus();
	}

	/**
	 * Implementation of a content proposal that contains a data object
	 * @param <F> the data object type
	 */
	private class ContentProposal<F> implements IContentProposal {
		private final F element;

		/**
		 * Constructor
		 * @param element
		 */
		public ContentProposal(F element) {
			this.element = element;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.fieldassist.IContentProposal#getContent()
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getContent() {
			return getProposalLabel((T) element);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.fieldassist.IContentProposal#getCursorPosition()
		 */
		@Override
		@SuppressWarnings("unchecked")
		public int getCursorPosition() {
			return getProposalLabel((T) element).length();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.fieldassist.IContentProposal#getDescription()
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getDescription() {
			return getProposalDescription((T) element);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.fieldassist.IContentProposal#getLabel()
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getLabel() {
			return getProposalLabel((T) element);
		}
	}

	/**
	 * @param filter
	 * @return the proposal data
	 */
	public abstract Collection<T> getProposalData(String filter);

	/**
	 * @param element
	 * @return the proposal label
	 */
	public abstract String getProposalLabel(T element);

	/**
	 * @param element
	 * @return the proposal description
	 */
	@SuppressWarnings("unused")
	public String getProposalDescription(T element) {
		return null;
	}

	/**
	 * Callback method to notify a listener about a selected proposal
	 * @param element
	 */
	@SuppressWarnings("unused")
	public void onProposalAccepted(T element) {

	}

	/**
	 * Set the layout data of the text field
	 * @param layoutData
	 */
	public void setLayoutData(Object layoutData) {
		internalTextField.setLayoutData(layoutData);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.fieldassist.IContentProposalProvider#getProposals(java.lang.String, int)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public IContentProposal[] getProposals(String contents, int position) {
		if (contents.length() < filterLength)
			return new ContentProposal[0];

		final Collection<T> elements = getProposalData(contents);
		final ContentProposal<T>[] proposals = new ContentProposal[elements.size()];
		int i = 0;

		for (final T element : elements)
			proposals[i++] = new ContentProposal<>(element);

		internalTextField.setData(elements);
		return proposals;
	}

	/**
	 * @param toolTipText
	 */
	public void setToolTipText(String toolTipText) {
		internalTextField.setToolTipText(toolTipText);
	}

}
