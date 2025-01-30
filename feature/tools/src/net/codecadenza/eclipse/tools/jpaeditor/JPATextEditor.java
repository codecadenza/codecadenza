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
package net.codecadenza.eclipse.tools.jpaeditor;

import net.codecadenza.eclipse.tools.jpaeditor.viewer.JPASyntax;
import net.codecadenza.eclipse.tools.jpaeditor.viewer.JPATextViewer;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;

/**
 * <p>
 * Implementation of a JPA text editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPATextEditor extends TextEditor {
	private final JPAQueryEditor editor;
	private JPATextViewer jpaTextViewer;
	private final JPASyntax syntax;
	private String lastSavedText = "";

	/**
	 * Constructor
	 * @param editor
	 * @param syntax
	 */
	public JPATextEditor(JPAQueryEditor editor, JPASyntax syntax) {
		this.editor = editor;
		this.syntax = syntax;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.texteditor.AbstractDecoratedTextEditor#isLineNumberRulerVisible()
	 */
	@Override
	protected boolean isLineNumberRulerVisible() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.texteditor.AbstractDecoratedTextEditor#isOverviewRulerVisible()
	 */
	@Override
	protected boolean isOverviewRulerVisible() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.texteditor.AbstractDecoratedTextEditor#isPrefQuickDiffAlwaysOn()
	 */
	@Override
	protected boolean isPrefQuickDiffAlwaysOn() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.texteditor.AbstractDecoratedTextEditor#isChangeInformationShowing()
	 */
	@Override
	public boolean isChangeInformationShowing() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#getProgressMonitor()
	 */
	@Override
	public IProgressMonitor getProgressMonitor() {
		return super.getProgressMonitor();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.editors.text.TextEditor#createActions()
	 */
	@Override
	protected void createActions() {
		super.createActions();

		final var action = new Action("Auto-Completion") {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.action.Action#run()
			 */
			@Override
			public void run() {
				jpaTextViewer.showAssistance();
			}
		};

		// This action definition is associated with the accelerator 'CTRL+SPACE'
		action.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
		setAction("ContentAssistProposal", action);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.texteditor.AbstractDecoratedTextEditor#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);

		final Object adapter = getAdapter(org.eclipse.swt.widgets.Control.class);

		if (adapter instanceof final StyledText styledText) {
			styledText.setWordWrap(false);

			lastSavedText = styledText.getText();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.texteditor.AbstractDecoratedTextEditor#createSourceViewer(org.eclipse.swt.widgets.Composite,
	 * org.eclipse.jface.text.source.IVerticalRuler, int)
	 */
	@Override
	protected ISourceViewer createSourceViewer(final Composite parent, IVerticalRuler ruler, int style) {
		parent.setLayout(new FillLayout());

		final var glViewerArea = new GridLayout();
		glViewerArea.marginHeight = glViewerArea.marginWidth = glViewerArea.horizontalSpacing = glViewerArea.verticalSpacing = 0;

		final var panViewerArea = new Composite(parent, SWT.NONE);
		panViewerArea.setLayout(glViewerArea);

		// Create the text viewer
		jpaTextViewer = new JPATextViewer(panViewerArea, style, syntax, ruler);
		jpaTextViewer.getControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		jpaTextViewer.getTextWidget().addVerifyKeyListener(event -> {
			if (event.stateMask == SWT.CTRL && event.keyCode == 13)
				event.doit = false;
		});

		jpaTextViewer.getTextWidget().addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyPressed(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent e) {
				JPATextEditor.this.editor.getEditorSite().getPage().activate(JPATextEditor.this.editor.getEditorSite().getPart());
			}
		});

		panViewerArea.layout();

		final var dc = new Document();
		jpaTextViewer.setDocument(dc);

		return jpaTextViewer;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#doSave(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void doSave(IProgressMonitor progressMonitor) {
		super.doSave(progressMonitor);

		this.lastSavedText = jpaTextViewer.getTextWidget().getText();
	}

	/**
	 * @return the text viewer
	 */
	JPATextViewer getJPATextViewer() {
		return this.jpaTextViewer;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#isDirty()
	 */
	@Override
	public boolean isDirty() {
		if (jpaTextViewer.getTextWidget() == null)
			return false;

		editor.setIsDirty(!lastSavedText.equals(jpaTextViewer.getTextWidget().getText()));

		return !lastSavedText.equals(jpaTextViewer.getTextWidget().getText());
	}

}
