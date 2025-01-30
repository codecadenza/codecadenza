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
package net.codecadenza.eclipse.diagram.domain.parsers;

import java.text.FieldPosition;
import java.text.MessageFormat;
import java.text.ParsePosition;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.diagram.domain.part.Messages;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.common.core.command.ICommand;
import org.eclipse.gmf.runtime.common.ui.services.parser.IParserEditStatus;
import org.eclipse.gmf.runtime.common.ui.services.parser.ParserEditStatus;
import org.eclipse.osgi.util.NLS;

/**
 * <p>
 * Message format parser
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MessageFormatParser extends AbstractParser {
	private String defaultPattern;
	private MessageFormat viewProcessor;
	private MessageFormat editorProcessor;
	private MessageFormat editProcessor;

	/**
	 * @param features
	 */
	public MessageFormatParser(EAttribute[] features) {
		super(features);
	}

	/**
	 * @return the default pattern
	 */
	protected String getDefaultPattern() {
		if (defaultPattern == null) {
			final var sb = new StringBuilder();

			for (int i = 0; i < features.length; i++) {
				if (i > 0)
					sb.append(' ');

				sb.append('{');
				sb.append(i);
				sb.append('}');
			}

			defaultPattern = sb.toString();
		}

		return defaultPattern;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.parsers.AbstractParser#getViewPattern()
	 */
	@Override
	public String getViewPattern() {
		final String pattern = super.getViewPattern();

		return pattern != null ? pattern : getDefaultPattern();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.parsers.AbstractParser#setViewPattern(java.lang.String)
	 */
	@Override
	public void setViewPattern(String viewPattern) {
		super.setViewPattern(viewPattern);

		viewProcessor = null;
	}

	/**
	 * @param viewPattern
	 * @return the message format
	 */
	protected MessageFormat createViewProcessor(String viewPattern) {
		return new MessageFormat(viewPattern);
	}

	/**
	 * @return the message format
	 */
	protected MessageFormat getViewProcessor() {
		if (viewProcessor == null)
			viewProcessor = createViewProcessor(getViewPattern());

		return viewProcessor;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.parsers.AbstractParser#getEditorPattern()
	 */
	@Override
	public String getEditorPattern() {
		final String pattern = super.getEditorPattern();
		return pattern != null ? pattern : getDefaultPattern();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.parsers.AbstractParser#setEditorPattern(java.lang.String)
	 */
	@Override
	public void setEditorPattern(String editorPattern) {
		super.setEditorPattern(editorPattern);
		editorProcessor = null;
	}

	/**
	 * @param editorPattern
	 * @return the message format
	 */
	protected MessageFormat createEditorProcessor(String editorPattern) {
		return new MessageFormat(editorPattern);
	}

	/**
	 * @return the message format
	 */
	protected MessageFormat getEditorProcessor() {
		if (editorProcessor == null)
			editorProcessor = createEditorProcessor(getEditorPattern());

		return editorProcessor;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.parsers.AbstractParser#getEditPattern()
	 */
	@Override
	public String getEditPattern() {
		final String pattern = super.getEditPattern();
		return pattern != null ? pattern : getDefaultPattern();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.parsers.AbstractParser#setEditPattern(java.lang.String)
	 */
	@Override
	public void setEditPattern(String editPattern) {
		super.setEditPattern(editPattern);
		editProcessor = null;
	}

	/**
	 * @param editPattern
	 * @return the message format
	 */
	protected MessageFormat createEditProcessor(String editPattern) {
		return new MessageFormat(editPattern);
	}

	/**
	 * @return the message format
	 */
	protected MessageFormat getEditProcessor() {
		if (editProcessor == null)
			editProcessor = createEditProcessor(getEditPattern());

		return editProcessor;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.ui.services.parser.IParser#getPrintString(org.eclipse.core.runtime.IAdaptable, int)
	 */
	@Override
	public String getPrintString(IAdaptable adapter, int flags) {
		final EObject element = adapter.getAdapter(EObject.class);
		return getViewProcessor().format(getValues(element), new StringBuffer(), new FieldPosition(0)).toString();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.ui.services.parser.IParser#getEditString(org.eclipse.core.runtime.IAdaptable, int)
	 */
	@Override
	public String getEditString(IAdaptable adapter, int flags) {
		final EObject element = adapter.getAdapter(EObject.class);
		return getEditorProcessor().format(getValues(element), new StringBuffer(), new FieldPosition(0)).toString();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.ui.services.parser.IParser#isValidEditString(org.eclipse.core.runtime.IAdaptable,
	 * java.lang.String)
	 */
	@Override
	public IParserEditStatus isValidEditString(IAdaptable adapter, String editString) {
		final var pos = new ParsePosition(0);
		final Object[] values = getEditProcessor().parse(editString, pos);

		if (values == null)
			return new ParserEditStatus(CodeCadenzaDiagramEditorPlugin.ID, IParserEditStatus.UNEDITABLE,
					NLS.bind(Messages.MessageFormatParser_InvalidInputError, Integer.valueOf(pos.getErrorIndex())));

		return validateNewValues(values);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.ui.services.parser.IParser#getParseCommand(org.eclipse.core.runtime.IAdaptable,
	 * java.lang.String, int)
	 */
	@Override
	public ICommand getParseCommand(IAdaptable adapter, String newString, int flags) {
		final Object[] values = getEditProcessor().parse(newString, new ParsePosition(0));
		return getParseCommand(adapter, values, flags);
	}

}
