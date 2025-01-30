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
package net.codecadenza.eclipse.tools.sqleditor.viewer;

import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN;
import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE;

import java.util.HashMap;
import java.util.Map;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.util.editor.WordPartDetector;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.swt.graphics.Image;

/**
 * <p>
 * Completion processor for SQL keywords, table and column names
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLCompletionProcessor implements IContentAssistProcessor {
	private final SQLSyntax syntax;
	private static final int TABLE_TYPE = 0;
	private static final int COLUMN_TYPE = 1;
	private static final int RES_WORD_TYPE = 2;

	/**
	 * Constructor
	 * @param syntax
	 */
	public SQLCompletionProcessor(SQLSyntax syntax) {
		this.syntax = syntax;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeCompletionProposals(org.eclipse.jface.text.
	 * ITextViewer, int)
	 */
	@Override
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int documentOffset) {
		final var proposalList = new HashMap<String, Integer>();
		final var wordPart = new WordPartDetector(viewer, documentOffset);
		ICompletionProposal[] result;
		int index = 0;
		final String text = viewer.getTextWidget().getText().substring(0, documentOffset);
		String token = text.isEmpty() ? text : text.substring(0, text.length() - 1);
		int charPos = -1;
		int lastCharPos = -1;

		for (final char c : token.toCharArray()) {
			charPos++;

			if (!Character.isLetterOrDigit(c) && c != '_' && c != '$' && c != '#' && c != '-')
				lastCharPos = charPos;
		}

		if (lastCharPos != -1)
			token = token.substring(lastCharPos + 1);

		if (syntax.getTableMap().containsKey(token)) {
			for (final String word : syntax.getTableMap().get(token))
				if (word.toLowerCase().startsWith(wordPart.getString().trim().toLowerCase()))
					proposalList.put(word, COLUMN_TYPE);
		}
		else {
			final String fullText = viewer.getTextWidget().getText();

			// Check if the word part represents an alias!
			for (final String tableName : syntax.getTableMap().keySet())
				if (fullText.contains(tableName + " " + token))
					syntax.getTableMap().get(tableName).forEach(colName -> proposalList.put(colName, COLUMN_TYPE));

			if (proposalList.isEmpty()) {
				// Iterate over all reserved words
				for (final String word : syntax.getReservedWords())
					if (word.toLowerCase().startsWith(wordPart.getString().trim().toLowerCase()))
						proposalList.put(word.toLowerCase(), RES_WORD_TYPE);

				for (final String word : syntax.getTableMap().keySet())
					if (word.toLowerCase().startsWith(wordPart.getString().trim().toLowerCase()))
						proposalList.put(word.toLowerCase(), TABLE_TYPE);
			}
		}

		result = new ICompletionProposal[proposalList.size()];

		// Create the proposals
		for (final Map.Entry<String, Integer> entry : proposalList.entrySet()) {
			// Create a new completion proposal
			final int proposalType = entry.getValue();
			final String proposal = entry.getKey();

			Image tmpImage = null;

			if (proposalType == COLUMN_TYPE)
				tmpImage = CodeCadenzaResourcePlugin.getImage(IMG_COLUMN);
			else if (proposalType == TABLE_TYPE)
				tmpImage = CodeCadenzaResourcePlugin.getImage(IMG_TABLE);

			result[index] = new CompletionProposal(proposal, wordPart.getOffset(), wordPart.getString().length(), proposal.length(),
					tmpImage, proposal, null, "");

			index++;
		}

		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getCompletionProposalAutoActivationCharacters()
	 */
	@Override
	public char[] getCompletionProposalAutoActivationCharacters() {
		return new char[] { '.' };
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeContextInformation(org.eclipse.jface.text.
	 * ITextViewer, int)
	 */
	@Override
	public IContextInformation[] computeContextInformation(ITextViewer arg0, int arg1) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationAutoActivationCharacters()
	 */
	@Override
	public char[] getContextInformationAutoActivationCharacters() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationValidator()
	 */
	@Override
	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getErrorMessage()
	 */
	@Override
	public String getErrorMessage() {
		return null;
	}

}
