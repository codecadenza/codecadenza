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
package net.codecadenza.runtime.richclient.eclipse.search.util;

import java.util.Collection;
import java.util.regex.Pattern;
import net.codecadenza.runtime.search.SearchService;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;

/**
 * <p>
 * Abstract base class for proposal providers that provide the selection of multiple items
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractMultiLOVProposalProvider implements IContentProposalProvider {
	private static final Pattern TOKEN_DELIMITER_PATTERN = Pattern.compile(SearchService.TOKEN_DELIMITER_IN);

	private final String command;

	/**
	 * Constructor
	 * @param command the list of values query command
	 */
	protected AbstractMultiLOVProposalProvider(String command) {
		this.command = command;
	}

	/**
	 * @return the service that is responsible for searching proposals
	 */
	protected abstract SearchService getSearchService();

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.fieldassist.IContentProposalProvider#getProposals(java.lang.String, int)
	 */
	@Override
	public IContentProposal[] getProposals(String contents, int position) {
		final String[] cont = TOKEN_DELIMITER_PATTERN.split(contents);
		String c = null;
		int start = 0;

		for (int a = 0; a < cont.length; a++) {
			final String s = cont[a];
			final int end = start + s.length();

			if (position == end) {
				c = s;
				cont[a] = null;
				break;
			}

			start = end + 2;
		}

		final SearchService searchService = getSearchService();
		final Collection<String> suggestions = searchService.getListOfValues(command, c);
		final var proposals = new IContentProposal[suggestions.size()];
		int i = 0;

		for (final String prop : suggestions) {
			proposals[i] = createProposal(prop, cont);
			i++;
		}

		return proposals;
	}

	/**
	 * Create the content proposal
	 * @param proposalString
	 * @param existingItems
	 * @return the proposal
	 */
	private IContentProposal createProposal(final String proposalString, final String[] existingItems) {
		return new IContentProposal() {
			private String prop = "";
			private int position;

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.fieldassist.IContentProposal#getContent()
			 */
			@Override
			public String getContent() {
				if (existingItems.length < 1) {
					prop = proposalString;
					position = prop.length();
					return prop;
				}

				for (final String existingItem : existingItems)
					if (existingItem != null)
						prop = prop + existingItem + ";;";
					else {
						prop = prop + proposalString + ";;";
						position = prop.length() - 2;
					}

				return prop;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.fieldassist.IContentProposal#getDescription()
			 */
			@Override
			public String getDescription() {
				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.fieldassist.IContentProposal#getLabel()
			 */
			@Override
			public String getLabel() {
				return proposalString;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.fieldassist.IContentProposal#getCursorPosition()
			 */
			@Override
			public int getCursorPosition() {
				return position;
			}
		};
	}

}
