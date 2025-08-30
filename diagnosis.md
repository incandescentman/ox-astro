## Issue Diagnosis

**Problem**: MDX output shows no images despite successful downloads and source updates

**Root Causes Identified:**
1. **Wrong MDX filename**: Generated as 'untitled-post.mdx' instead of 'image-link-test.mdx'
2. **Local images not updated in source**: Still point to /Users/jay/Downloads/ instead of asset paths  
3. **Path collection mismatch**: Collection happens before local image paths are updated
4. **Re-collection not working**: System doesn't re-scan updated source buffer properly

**Status:**  
✅ Remote images: Download + source update working  
❌ Local images: Copy working but source update failing  
❌ Final collection: Not finding updated paths for import generation

**Next Steps:**
1. Fix local image source buffer updates
2. Ensure re-collection scans updated buffer 
3. Fix MDX filename generation issue
